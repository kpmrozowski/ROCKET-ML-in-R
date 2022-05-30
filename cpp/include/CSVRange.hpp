#include <iterator>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <regex>
#include <boost/range/algorithm/copy.hpp>

namespace wztum
{

// https://stackoverflow.com/questions/1120140/how-can-i-read-and-parse-csv-files-in-c
class CSVRow
{
    public:
        std::string operator[](std::size_t index) const
        {
            return std::string(&m_line[m_data[index] + 1], m_data[index + 1] -  (m_data[index] + 1));
        }
        std::size_t size() const
        {
            return m_data.size() - 1;
        }
        void readNextRow(std::istream& str)
        {
            std::getline(str, m_line);
            // boost::copy(m_line, std::ostream_iterator<char>(std::cout, ""));
            m_line = std::regex_replace(m_line, std::regex("[ \\t]+"), ",");
            m_line = std::regex_replace(m_line, std::regex("NaN"), "0.0");
            m_line = std::regex_replace(m_line, std::regex("\\r\\n,|\\r,|\\n,"), "\r\n");
            if (m_line[0] == ',')
            {
               m_line.erase(0,1);
            }
            // std::cout << std::endl;
            // boost::copy(m_line, std::ostream_iterator<char>(std::cout, ""));
            // std::cout << std::endl;

            m_data.clear();
            m_data.emplace_back(-1);
            std::string::size_type pos = 0;
            while((pos = m_line.find(',', pos)) != std::string::npos)
            {
                m_data.emplace_back(pos);
                ++pos;
            }
            // This checks for a trailing comma with no data after it.
            pos   = m_line.size();
            m_data.emplace_back(pos);
        }
    private:
        std::string         m_line;
        std::vector<int>    m_data;
};

std::istream& operator>>(std::istream& str, CSVRow& data)
{
    data.readNextRow(str);
    return str;
}

class CSVIterator
{
public:
    typedef std::input_iterator_tag iterator_category;
    typedef CSVRow value_type;
    typedef std::size_t difference_type;
    typedef CSVRow* pointer;
    typedef CSVRow& reference;

    CSVIterator(std::istream& str) : m_str(str.good() ? &str : NULL)
    {
        ++(*this);
    }
    CSVIterator() : m_str(NULL) {}

    // Pre Increment
    CSVIterator& operator++()
    {
        if (m_str)
        {
            if (!((*m_str) >> m_row))
            {
                m_str = NULL;
            }
        }
        return *this;
    }
    // Post increment
    CSVIterator operator++(int)
    {
        CSVIterator tmp(*this);
        ++(*this);
        return tmp;
    }
    // CSVIterator operator[](int n) const
    // {
    //     return std::next(*this, n);
    // }
    CSVRow const& operator*() const { return m_row; }
    CSVRow const* operator->() const { return &m_row; }

    bool operator==(CSVIterator const& rhs) const
    {
        return ((this == &rhs) ||
                ((this->m_str == NULL) && (rhs.m_str == NULL)));
    }
    bool operator!=(CSVIterator const& rhs) const { return !((*this) == rhs); }

private:
    std::istream* m_str;
    CSVRow m_row;
};

class CSVRange
{
    std::istream& stream;

public:
    CSVRange(std::istream& str) : stream(str) {}
    CSVIterator begin() const { return CSVIterator{stream}; }
    // CSVIterator operator[](int n) const
    // {
    //     return std::next(CSVIterator{stream}, n);
    // }
    CSVIterator end() const { return CSVIterator{}; }

   std::vector<std::vector<float>> to_vector_f()
   {
      std::vector<std::vector<float>> v;
      for (auto& row : *this)
      {
         std::vector<float> sv{};
         for (int i = 0; i < row.size(); ++i)
         {
               sv.push_back(std::stod(row[i]));
         }
         v.push_back(sv);
      }
      return v;
   }

   std::vector<std::string> to_vector_s()
   {
      std::vector<std::string> v;
      for (auto& row : *this)
      {
         v.push_back(row[0]);
      }
      return v;
   }
};

}
