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

extern "C"{
  void c_doAsr(void* argsPtr, int len, int16* dataPtr);
}

void GetDiagnosticsAndPrintOutput(const std::string &utt,
                                  const fst::SymbolTable *word_syms,
                                  const CompactLattice &clat,
                                  int64 *tot_num_frames,
                                  double *tot_like) {
  std::cout << "Diag start";
  if (clat.NumStates() == 0) {
    KALDI_WARN << "Empty lattice.";
    std::cout << "Early exit";
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

  std::cout << std::endl;
  if (word_syms != NULL) {
    std::cerr << utt << ' ';
    for (size_t i = 0; i < words.size(); i++) {
      std::string s = word_syms->Find(words[i]);
      if (s == "")
        KALDI_ERR << "Word-id " << words[i] << " not in symbol table.";
      std::cout  << "EntryStart (" << words[i] << ")=" ;
      std::cout << s << ' ';
      std::cout  << " EntryEnd";
      std::cout << std::endl;
    }
    std::cout << std::endl;
  } else {
    std::cout  << "Word SYm null";
  }
}

void* init_kaldi();
extern "C" {
  void* c_init_kaldi () {
    void* ptr = init_kaldi();
  }
}

void* init_kaldi() {
  try {
    using namespace kaldi;
    using namespace fst;

    typedef kaldi::int32 int32;
    typedef kaldi::int64 int64;

    std::string word_syms_rxfilename = "exp/tdnn_7b_chain_online/graph_pp/words.txt";

    // feature_opts includes configuration for the iVector adaptation,
    // as well as the basic features.
    nnet3::NnetSimpleLoopedComputationOptions *decodable_opts = new nnet3::NnetSimpleLoopedComputationOptions;
    (*decodable_opts).frame_subsampling_factor = 3;
    (*decodable_opts).acoustic_scale = 1.0;

    BaseFloat chunk_length_secs = 0.18;
    bool do_endpointing = false;
    bool online = false;


    std::string nnet3_rxfilename = "exp/tdnn_7b_chain_online/final.mdl",
      fst_rxfilename = "exp/tdnn_7b_chain_online/graph_pp/HCLG.fst";


    TransitionModel* trans_model = new TransitionModel;
    nnet3::AmNnetSimple* am_nnet = new nnet3::AmNnetSimple;
    {
      bool binary;
      Input ki(nnet3_rxfilename, &binary);
      trans_model->Read(ki.Stream(), binary);
      (*am_nnet).Read(ki.Stream(), binary);
      SetBatchnormTestMode(true, &(am_nnet->GetNnet()));
      SetDropoutTestMode(true, &(am_nnet->GetNnet()));
      // DIVAM
      //nnet3::CollapseModel(nnet3::CollapseModelConfig(), &(am_nnet.GetNnet()));
    }

    // this object contains precomputed stuff that is used by all decodable
    // objects.  It takes a pointer to am_nnet because if it has iVectors it has
    // to modify the nnet to accept iVectors at intervals.
    nnet3::DecodableNnetSimpleLoopedInfo* decodable_info =
      new nnet3::DecodableNnetSimpleLoopedInfo(*decodable_opts,
                                                        am_nnet);


    fst::Fst<fst::StdArc> *decode_fst = fst::ReadFstKaldi(fst_rxfilename);

    fst::SymbolTable *word_syms = NULL;
    if (word_syms_rxfilename != "")
      if (!(word_syms = fst::SymbolTable::ReadText(word_syms_rxfilename)))
        KALDI_ERR << "Could not read symbol table from file "
                  << word_syms_rxfilename;

    DoAsrArgs* doAsrArgs = new DoAsrArgs;

    (*doAsrArgs).trans_model = trans_model;
    (*doAsrArgs).decode_fst = decode_fst;
    (*doAsrArgs).decodable_info = decodable_info;
    (*doAsrArgs).decodable_opts = decodable_opts;
    (*doAsrArgs).word_syms = word_syms;

    return ((void*) (doAsrArgs));
  } catch(const std::exception& e) {
    std::cerr << e.what();
    return NULL;
  }
} // end


void c_doAsr(void* argsPtr, int len, int16* dataPtr) {
  DoAsrArgs doAsrArgs = *((DoAsrArgs*) argsPtr);

  // std::cout<< "Size=" << sizeof(float);
  float* dataFloat = new float[len];
  for (int i = 0; i < len; ++i) {
    int16 k = *(dataPtr + i);
    *(dataFloat+i) = k;
  }

  SubVector<float> data(dataFloat,len);

  FrameExtractionOptions frameExtOpts;
  frameExtOpts.samp_freq = 8000;

  MelBanksOptions melBankOpts;
  melBankOpts.num_bins = 40;
  melBankOpts.low_freq = 40;
  melBankOpts.high_freq = -200;


  MfccOptions mfccOptions;
  mfccOptions.num_ceps = 40;
  mfccOptions.use_energy = false;

  mfccOptions.mel_opts = melBankOpts;
  mfccOptions.frame_opts = frameExtOpts;

  // OnlineIvectorExtractionConfig ivectorConf;
  // ivectorConf.lda_mat_rxfilename = "exp/tdnn_7b_chain_online/ivector_extractor/final.mat";
  // ivectorConf.global_cmvn_stats_rxfilename = "exp/tdnn_7b_chain_online/ivector_extractor/global_cmvn.stats";
  // ivectorConf.splice_config_rxfilename = "exp/tdnn_7b_chain_online/conf/splice.conf";
  // ivectorConf.cmvn_config_rxfilename = "exp/tdnn_7b_chain_online/conf/online_cmvn.conf";
  // ivectorConf.ivector_extractor_rxfilename = "exp/tdnn_7b_chain_online/ivector_extractor/final.ie";

  // OnlineIvectorExtractionInfo ivectorExtractorInfo(ivectorConf);
  // ivectorExtractorInfo.num_gselect = 5;
  // ivectorExtractorInfo.min_post = 0.025;
  // ivectorExtractorInfo.posterior_scale = 0.1;
  // ivectorExtractorInfo.max_remembered_frames = 1000;
  // ivectorExtractorInfo.max_count = 100;

  OnlineNnet2FeaturePipelineConfig feature_opts;
  feature_opts.ivector_extraction_config = "exp/tdnn_7b_chain_online/conf/ivector_extractor.conf";
  OnlineNnet2FeaturePipelineInfo feature_info(feature_opts);

  feature_info.mfcc_opts = mfccOptions;
  feature_info.feature_type = "mfcc";

  LatticeFasterDecoderConfig decoder_opts;
  decoder_opts.lattice_beam = 6.0;
  decoder_opts.max_active = 7000;

  doAsrArgs.feature_info = &feature_info;
  doAsrArgs.decoder_opts = &decoder_opts;

  std::cout << "Feature-Type=" << (*(doAsrArgs.feature_info)).feature_type;
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
    // int32 chunk_length = 1;
    // int32 samp_offset = 0;
    int32 samp_freq = 8000;


    // while (samp_offset < data.Dim()) {
    //   int32 samp_remaining = data.Dim() - samp_offset;
    //   int32 num_samp = chunk_length < samp_remaining ? chunk_length
    //                                   : samp_remaining;

    //   // SubVector<BaseFloat> wave_part(data, samp_offset, num_samp);
    //   feature_pipeline.AcceptWaveform(samp_freq, data);

    //   samp_offset += num_samp;
    //   if (samp_offset == data.Dim()) {
    //     // no more input. flush out last frames
    //     feature_pipeline.InputFinished();
    //   }

    //   decoder.AdvanceDecoding();
    // }
    feature_pipeline.AcceptWaveform(samp_freq, data);
    decoder.AdvanceDecoding();
    feature_pipeline.InputFinished();
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
    return;
  } catch(const std::exception& e) {
    std::cerr << e.what();
    return;
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

g++ -std=c++11 -c -o minimal.o -DHAVE_ATLAS -I/nix/store/5qri2g3kw6zl94dl5c27qkal0fcjh0xz-kaldi-5.2.5/include -I/nix/store/8mh4r08nziknq31xk42gkgy2sx5n5yjv-openfst-1.6.3/include -I/nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/include minimal.cpp

g++  -g -std=c++11 -c -o online.o -DHAVE_ATLAS -I/nix/store/5qri2g3kw6zl94dl5c27qkal0fcjh0xz-kaldi-5.2.5/include -I/nix/store/8mh4r08nziknq31xk42gkgy2sx5n5yjv-openfst-1.6.3/include -I/nix/store/xr7ylbmlskkjcbscj00qxz4y9119rrc5-atlas-3.10.2/include online2-wav-nnet3-latgen-faster.cc

ghc -dynamic minimal.o online.o Main.hs -lkaldi-hmm -lkaldi-online2 -lkaldi-lat -lkaldi-util -lkaldi-kws -lkaldi-decoder -lkaldi-gmm -lkaldi-transform -lkaldi-chain -lkaldi-nnet2 -lkaldi-nnet -lkaldi-sgmm2 -lkaldi-cudamatrix -lkaldi-base -lkaldi-fstext -lkaldi-lm -lkaldi-nnet3 -lkaldi-ivector -lkaldi-matrix -lkaldi-feat -lkaldi-tree

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
