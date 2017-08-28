#include "fstext/kaldi-fst-io.h"
#include "feat/wave-reader.h"
#include "online2/online-nnet3-decoding.h"
#include "online2/online-nnet2-feature-pipeline.h"
#include "online2/onlinebin-util.h"
#include "online2/online-timing.h"
#include "online2/online-endpoint.h"
#include "fstext/fstext-lib.h"
#include "lat/lattice-functions.h"
#include "util/kaldi-thread.h"
#include "nnet3/nnet-utils.h"

using namespace kaldi;

struct DoAsrArgs {
  OnlineNnet2FeaturePipelineInfo* feature_info;
  TransitionModel* trans_model;
  fst::Fst<fst::StdArc> *decode_fst;
  nnet3::DecodableNnetSimpleLoopedInfo* decodable_info;
  nnet3::NnetSimpleLoopedComputationOptions* decodable_opts;
  LatticeFasterDecoderConfig* decoder_opts;
  fst::SymbolTable *word_syms;
};

int doAsr(SubVector<BaseFloat>& data, DoAsrArgs& doAsrArgs);

void GetDiagnosticsAndPrintOutput(const std::string &utt,
                                  const fst::SymbolTable *word_syms,
                                  const CompactLattice &clat,
                                  int64 *tot_num_frames,
                                  double *tot_like) {
  if (clat.NumStates() == 0) {
    KALDI_WARN << "Empty lattice.";
    return;
  }
  CompactLattice best_path_clat;
  CompactLatticeShortestPath(clat, &best_path_clat);

  Lattice best_path_lat;
  ConvertLattice(best_path_clat, &best_path_lat);

  double likelihood;
  LatticeWeight weight;
  int32 num_frames;
  std::vector<int32> alignment;
  std::vector<int32> words;
  GetLinearSymbolSequence(best_path_lat, &alignment, &words, &weight);
  num_frames = alignment.size();
  likelihood = -(weight.Value1() + weight.Value2());
  *tot_num_frames += num_frames;
  *tot_like += likelihood;
  KALDI_VLOG(2) << "Likelihood per frame for utterance " << utt << " is "
                << (likelihood / num_frames) << " over " << num_frames
                << " frames.";

  if (word_syms != NULL) {
    std::cerr << utt << ' ';
    for (size_t i = 0; i < words.size(); i++) {
      std::string s = word_syms->Find(words[i]);
      if (s == "")
        KALDI_ERR << "Word-id " << words[i] << " not in symbol table.";
      std::cerr << s << ' ';
    }
    std::cerr << std::endl;
  }
}

int c_main_int();
extern "C" {
  int c_main () {
    c_main_int();
  }
}

int c_main_int() {
  try {
    using namespace kaldi;
    using namespace fst;

    typedef kaldi::int32 int32;
    typedef kaldi::int64 int64;

    const char *usage =
        "Reads in wav file(s) and simulates online decoding with neural nets\n"
        "(nnet3 setup), with optional iVector-based speaker adaptation and\n"
        "optional endpointing.  Note: some configuration values and inputs are\n"
        "set via config files whose filenames are passed as options\n"
        "\n"
        "Usage: online2-wav-nnet3-latgen-faster [options] <nnet3-in> <fst-in> "
        "<spk2utt-rspecifier> <wav-rspecifier> <lattice-wspecifier>\n"
        "The spk2utt-rspecifier can just be <utterance-id> <utterance-id> if\n"
        "you want to decode utterance by utterance.\n";

    std::string word_syms_rxfilename = "exp/tdnn_7b_chain_online/graph_pp/words.txt";

    // feature_opts includes configuration for the iVector adaptation,
    // as well as the basic features.
    OnlineNnet2FeaturePipelineConfig feature_opts;
    nnet3::NnetSimpleLoopedComputationOptions decodable_opts;
    LatticeFasterDecoderConfig decoder_opts;

    BaseFloat chunk_length_secs = 0.18;
    bool do_endpointing = false;
    bool online = false;


    std::string nnet3_rxfilename = "exp/tdnn_7b_chain_online/final.mdl",
        fst_rxfilename = "exp/tdnn_7b_chain_online/graph_pp/HCLG.fst",
        spk2utt_rspecifier = "ark:echo utterance-id1 utterance-id1|",
        wav_rspecifier = "scp:echo utterance-id1 ~/lenovo/try.wav|",
        clat_wspecifier = "ark:/dev/null";

    OnlineNnet2FeaturePipelineInfo feature_info(feature_opts);

    if (!online) {
      feature_info.ivector_extractor_info.use_most_recent_ivector = true;
      feature_info.ivector_extractor_info.greedy_ivector_extractor = true;
      chunk_length_secs = -1.0;
    }

    TransitionModel trans_model;
    nnet3::AmNnetSimple am_nnet;
    {
      bool binary;
      Input ki(nnet3_rxfilename, &binary);
      trans_model.Read(ki.Stream(), binary);
      am_nnet.Read(ki.Stream(), binary);
      SetBatchnormTestMode(true, &(am_nnet.GetNnet()));
      SetDropoutTestMode(true, &(am_nnet.GetNnet()));
      // DIVAM
      //nnet3::CollapseModel(nnet3::CollapseModelConfig(), &(am_nnet.GetNnet()));
    }

    // this object contains precomputed stuff that is used by all decodable
    // objects.  It takes a pointer to am_nnet because if it has iVectors it has
    // to modify the nnet to accept iVectors at intervals.
    nnet3::DecodableNnetSimpleLoopedInfo decodable_info(decodable_opts,
                                                        &am_nnet);


    fst::Fst<fst::StdArc> *decode_fst = fst::ReadFstKaldi(fst_rxfilename);

    fst::SymbolTable *word_syms = NULL;
    if (word_syms_rxfilename != "")
      if (!(word_syms = fst::SymbolTable::ReadText(word_syms_rxfilename)))
        KALDI_ERR << "Could not read symbol table from file "
                  << word_syms_rxfilename;

    int32 num_done = 0, num_err = 0;
    double tot_like = 0.0;
    int64 num_frames = 0;

    SequentialTokenVectorReader spk2utt_reader(spk2utt_rspecifier);
    RandomAccessTableReader<WaveHolder> wav_reader(wav_rspecifier);
    OnlineTimingStats timing_stats;

    float dataPtr = 12.1231;
    SubVector<float> data(&dataPtr,1);

    MfccOptions mfccOptions;
    mfccOptions.num_ceps = 40;
    feature_info.mfcc_opts = mfccOptions;

    DoAsrArgs doAsrArgs;
    doAsrArgs.feature_info = &feature_info;
    doAsrArgs.trans_model = &trans_model;
    doAsrArgs.decode_fst = decode_fst;
    doAsrArgs.decodable_opts = &decodable_opts;
    doAsrArgs.decodable_info = &decodable_info;
    doAsrArgs.decoder_opts = &decoder_opts;
    doAsrArgs.word_syms = word_syms;

    return doAsr(data,doAsrArgs);
  } catch(const std::exception& e) {
    std::cerr << e.what();
    return -1;
  }
} // end


int doAsr(SubVector<BaseFloat>& data, DoAsrArgs& doAsrArgs) {
  try {
    using namespace kaldi;
    using namespace fst;

    typedef kaldi::int32 int32;
    typedef kaldi::int64 int64;

    OnlineNnet2FeaturePipeline feature_pipeline(*(doAsrArgs.feature_info));

    OnlineSilenceWeighting silence_weighting(
                                             *(doAsrArgs.trans_model),
                                             (*(doAsrArgs.feature_info)).silence_weighting_config,
                                             (*(doAsrArgs.decodable_opts)).frame_subsampling_factor);

    SingleUtteranceNnet3Decoder decoder(*(doAsrArgs.decoder_opts),
                                        *(doAsrArgs.trans_model),
                                        (*(doAsrArgs.decodable_info)),
                                        *(doAsrArgs.decode_fst), &feature_pipeline);
    int32 chunk_length = 50;
    int32 samp_offset = 0;
    int32 samp_freq = 16000;


    while (samp_offset < data.Dim()) {
      int32 samp_remaining = data.Dim() - samp_offset;
      int32 num_samp = chunk_length < samp_remaining ? chunk_length
                                      : samp_remaining;

      SubVector<BaseFloat> wave_part(data, samp_offset, num_samp);
      feature_pipeline.AcceptWaveform(samp_freq, wave_part);

      samp_offset += num_samp;
      if (samp_offset == data.Dim()) {
        // no more input. flush out last frames
        feature_pipeline.InputFinished();
      }

      decoder.AdvanceDecoding();
    }
    decoder.FinalizeDecoding();

    CompactLattice clat;
    bool end_of_utterance = true;
    decoder.GetLattice(end_of_utterance, &clat);

    int64 num_frames = 2;
    double tot_like = 2;
    std::string utt = "UtteranceName";
    GetDiagnosticsAndPrintOutput(utt, doAsrArgs.word_syms, clat,
                                 &num_frames, &tot_like);

    // we want to output the lattice with un-scaled acoustics.
    BaseFloat inv_acoustic_scale =
      1.0 / (*(doAsrArgs.decodable_opts)).acoustic_scale;
    ScaleLattice(AcousticLatticeScale(inv_acoustic_scale), &clat);

    KALDI_LOG << "Decoded utterance " << utt;

    //delete decode_fst;
    //delete word_syms; // will delete if non-NULL.
    return 1;
  } catch(const std::exception& e) {
    std::cerr << e.what();
    return -1;
  }
} // main()

/*

  /nix/store/2m6v6lnmnf9521ixd5v0x4qrmjkn30ws-gcc-5.4.0/bin/g++ -std=c++11 -I.. -I/nix/store/8mh4r08nziknq31xk42gkgy2sx5n5yjv-openfst-1.6.3/include  -Wall -Wno-sign-compare -Wno-unused-local-typedefs -Wno-deprecated-declarations -Winit-self -DKALDI_DOUBLEPRECISION=0 -DHAVE_EXECINFO_H=1 -DHAVE_CXXABI_H -DHAVE_ATLAS -I/nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/include -msse -msse2 -pthread -g  -fPIC -DHAVE_CUDA -I/nix/store/fk9bhb0ry0ba11lmsfk9kddi4qn0cxra-cudatoolkit-8.0.44/include   -c -o online2-wav-nnet3-latgen-faster.o online2-wav-nnet3-latgen-faster.cc

/nix/store/2m6v6lnmnf9521ixd5v0x4qrmjkn30ws-gcc-5.4.0/bin/g++  -Wl,-rpath=/nix/store/8mh4r08nziknq31xk42gkgy2sx5n5yjv-openfst-1.6.3/lib -rdynamic -L/nix/store/fk9bhb0ry0ba11lmsfk9kddi4qn0cxra-cudatoolkit-8.0.44/lib64 -Wl,-rpath,/nix/store/fk9bhb0ry0ba11lmsfk9kddi4qn0cxra-cudatoolkit-8.0.44/lib64 -Wl,-rpath=/tmp/nix-build-kaldi-5.2.5.drv-3/kaldi-3e57783/src/lib  online2-wav-nnet3-latgen-faster.o   ../online2/libkaldi-online2.so  ../ivector/libkaldi-ivector.so  ../nnet3/libkaldi-nnet3.so  ../chain/libkaldi-chain.so  ../nnet2/libkaldi-nnet2.so  ../cudamatrix/libkaldi-cudamatrix.so  ../decoder/libkaldi-decoder.so  ../lat/libkaldi-lat.so  ../fstext/libkaldi-fstext.so  ../hmm/libkaldi-hmm.so  ../feat/libkaldi-feat.so  ../transform/libkaldi-transform.so  ../gmm/libkaldi-gmm.so  ../tree/libkaldi-tree.so  ../util/libkaldi-util.so  ../matrix/libkaldi-matrix.so  ../base/libkaldi-base.so /nix/store/8mh4r08nziknq31xk42gkgy2sx5n5yjv-openfst-1.6.3/lib/libfst.so /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/liblapack_atlas.a /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/libcblas.a /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/libatlas.a /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/libf77blas.a -lm -lpthread -ldl -lcublas -lcudart -lcurand  -o online2-wav-nnet3-latgen-faster

/nix/store/2m6v6lnmnf9521ixd5v0x4qrmjkn30ws-gcc-5.4.0/bin/g++  -Wl,-rpath=/nix/store/8mh4r08nziknq31xk42gkgy2sx5n5yjv-openfst-1.6.3/lib\
 -rdynamic -L/nix/store/fk9bhb0ry0ba11lmsfk9kddi4qn0cxra-cudatoolkit-8.0.44/lib64 -Wl,-rpath,/nix/store/fk9bhb0ry0ba11lmsfk9kddi4qn0cxra-cudatoolkit-8.0.44/lib64\
 -Wl,-rpath=/nix/store/5qri2g3kw6zl94dl5c27qkal0fcjh0xz-kaldi-5.2.5/lib  minimal.o\
 -Wl,-L/nix/store/5qri2g3kw6zl94dl5c27qkal0fcjh0xz-kaldi-5.2.5/lib\
 -lkaldi-online2\
 -L/nix/store/5qri2g3kw6zl94dl5c27qkal0fcjh0xz-kaldi-5.2.5/lib\
 -lkaldi-ivector  -lkaldi-nnet3  -lkaldi-chain  -lkaldi-nnet2  -lkaldi-cudamatrix  -lkaldi-decoder  -lkaldi-lat  -lkaldi-fstext \
 -lkaldi-hmm  -lkaldi-feat  -lkaldi-transform  -lkaldi-gmm  -lkaldi-tree  -lkaldi-util  -lkaldi-matrix  -lkaldi-base -lfst /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/liblapack_atlas.a /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/libcblas.a /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/libatlas.a /nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/lib/libf77blas.a -lm -lpthread -ldl -lcublas -lcudart -lcurand  -o minimal-exe


 g++ -std=c++11 -c -o minimal.o -DHAVE_ATLAS -I/nix/store/5qri2g3kw6zl94dl5c27qkal0fcjh0xz-kaldi-5.2.5/include -I/nix/store/8mh4r08nziknq31xk42gkgy2sx5n5yjv-openfst-1.6.3/include -I/nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/include minimal.cpp |& head

 ghc -dynamic minimal.o Main.hs\
 -lkaldi-hmm\
 -lkaldi-online2\
 -lkaldi-lat\
 -lkaldi-util\
 -lkaldi-kws\
 -lkaldi-decoder\
 -lkaldi-gmm\
 -lkaldi-transform\
 -lkaldi-chain\
 -lkaldi-nnet2\
 -lkaldi-nnet\
 -lkaldi-sgmm2\
 -lkaldi-cudamatrix\
 -lkaldi-base\
 -lkaldi-fstext\
 -lkaldi-lm\
 -lkaldi-nnet3\
 -lkaldi-ivector\
 -lkaldi-matrix\
 -lkaldi-feat\
 -lkaldi-tree
 */
