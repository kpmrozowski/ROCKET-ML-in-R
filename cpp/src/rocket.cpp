#include <CSVRange.hpp>
#include <csvfile.hpp>
#include <fmt/core.h>
#include <argparse.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <mlpack/methods/linear_regression/linear_regression.hpp>

#define pr(x) fmt::print("{}\n", x)


using CsvF = std::vector<std::vector<float>>;
using CsvS = std::vector<std::string>;
using std::string;

// std::pair<CsvF, std::vector<float>> load_train_test(string path, string dataset_name)
// {
//    string test_path = path + "/" + dataset_name + "/" + dataset_name + "_TEST.txt";
//    std::ifstream file_test(test_path);
//    CsvF csv = wztum::CSVRange(file_test).to_vector_f();
//    fmt::print("test size: ({},{})\n", csv.size(), csv[0].size());
//    auto X_test = boost::adaptors::transform(csv, [](std::vector<float> v){ v.erase(v.begin()); return v; });
//    auto Y_test = boost::adaptors::transform(csv, [](std::vector<float>& v){ return v[0]; });
//    return X_test, Y_test;
// }

struct MyArgs : public argparse::Args {
    std::string &dataset_names  = kwarg("d,dataset_names", "txt file of dataset names").set_default("/home/kmro/Documents/pw_mini/S3/wztum/rocket-cpp/data/Cricket.txt");
    std::string &input_path     = kwarg("i,input_path", "parent directory for datasets").set_default("/home/kmro/Documents/pw_mini/S3/wztum/rocket-cpp/data");
    std::string &output_path    = kwarg("o,output_path", "path for results").set_default("/home/kmro/Documents/pw_mini/S3/wztum/rocket-cpp/results");
    int &num_runs               = kwarg("n,num_runs", "number of runs (optional, default 10)").set_default(10);
    int &num_kernels            = kwarg("k,num_kernels", "number of kernels (optional, default 10,000)").set_default(10'000);
    bool &verbose               = flag("v,verbose", "A flag to toggle verbose").set_default(false);
    // std::string &src_path  = arg("a positional string argument");
};

int main(int argc, char** argv)
{
    fmt::print("hello\n");

    // # == parse arguments ===========================================================
    
    auto args = argparse::parse<MyArgs>(argc, argv);
    std::ifstream file_dataset_names{args.dataset_names};
    CsvS dataset_names = wztum::CSVRange(file_dataset_names).to_vector_s();
    wztum::csvfile results(args.output_path + "/results_cpp.csv", ",");

    for (const string& dataset_name : dataset_names)
    {
        // -- read data -------------------------------------------------------------
        
        string test_path = args.input_path + "/" + dataset_name + "/" + dataset_name + "_TEST.txt";
        std::ifstream file_test(test_path);
        CsvF csv = wztum::CSVRange(file_test).to_vector_f();
        fmt::print("test size: ({},{})\n", csv.size(), csv[0].size());
      //   auto X_test = boost::adaptors::transform(csv, [](std::vector<float> v){ v.erase(v.begin()); return v; });
      //   auto Y_test = boost::adaptors::transform(csv, [](std::vector<float>& v){ return v[0]; });
      //   pr(X_test[600][1]);
      //   pr(Y_test[600]);
        string train_path = args.input_path + "/" + dataset_name + "/" + dataset_name + "_TRAIN.txt";
        std::ifstream file_train(train_path);
        csv = wztum::CSVRange(file_train).to_vector_f();
        fmt::print("train size: ({},{})\n", csv.size(), csv[0].size());
      //   auto X_train = boost::adaptors::transform(csv, [](std::vector<float> v){ v.erase(v.begin()); return v; });
      //   auto Y_train = boost::adaptors::transform(csv, [](std::vector<float>& v){ return v[0]; });
      //   pr(X_train[100][1]);
      //   pr(Y_train[100]);
        pr("Done");

        // -- run -------------------------------------------------------------------

        results << "dataset"
                << "accuracy_mean"
                << "accuracy_standard_deviation"
                << "time_training_seconds"
                << "time_test_seconds" << wztum::endrow;


        // boost::copy(vec, std::ostream_iterator<float>(std::cout, ","));

        // const std::string separator = ",";
        // inkub::csvfile csv(dirPath + "results.csv", separator);
        // csv << "aa"
        //     << "bb"
        //     << "cc" << inkub::endrow;
    }
    return 0;
}