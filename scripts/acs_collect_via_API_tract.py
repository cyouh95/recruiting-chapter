import requests
import json
import csv
import os


# Doc: https://www.census.gov/data/developers/data-sets/acs-5year.2017.html
# Variables: https://api.census.gov/data/2017/acs/acs5/variables.html
# Geography: https://api.census.gov/data/2017/acs/acs5/geography.html


def main():

    col_names = ['tract_name',
    
                 # Population
                 'pop_total', 'pop_total_25plus',

                 # Median household income
                 'median_household_income', 'median_inc_2544', 'median_inc_4564',
    
                 # Race and ethnicity
                 'pop_white_m_15-17', 'pop_white_m_18-19', 'pop_white_f_15-17', 'pop_white_f_18-19',
                 'pop_black_m_15-17', 'pop_black_m_18-19', 'pop_black_f_15-17', 'pop_black_f_18-19',
                 'pop_amerindian_m_15-17', 'pop_amerindian_m_18-19', 'pop_amerindian_f_15-17', 'pop_amerindian_f_18-19',
                 'pop_asian_m_15-17', 'pop_asian_m_18-19', 'pop_asian_f_15-17', 'pop_asian_f_18-19',
                 'pop_nativehawaii_m_15-17', 'pop_nativehawaii_m_18-19', 'pop_nativehawaii_f_15-17', 'pop_nativehawaii_f_18-19',
                 'pop_otherrace_m_15-17', 'pop_otherrace_m_18-19', 'pop_otherrace_f_15-17', 'pop_otherrace_f_18-19',
                 'pop_tworaces_m_15-17', 'pop_tworaces_m_18-19', 'pop_tworaces_f_15-17', 'pop_tworaces_f_18-19',
                 'pop_hispanic_m_15-17', 'pop_hispanic_m_18-19', 'pop_hispanic_f_15-17', 'pop_hispanic_f_18-19',
                 
                 'fips_state_code', 'fips_county_code', 'tract']  # last by default

    var_names = ['NAME',
    
                 # Population
                 'B01003_001E', 'B15003_001E',

                 # Median household income
                 'B19013_001E', 'B19049_003E', 'B19049_004E',
    
                 # Race and ethnicity
                 'B01001H_006E', 'B01001H_007E', 'B01001H_021E', 'B01001H_022E',
                 'B01001B_006E', 'B01001B_007E', 'B01001B_021E', 'B01001B_022E',
                 'B01001C_006E', 'B01001C_007E', 'B01001C_021E', 'B01001C_022E',
                 'B01001D_006E', 'B01001D_007E', 'B01001D_021E', 'B01001D_022E',
                 'B01001E_006E', 'B01001E_007E', 'B01001E_021E', 'B01001E_022E',
                 'B01001F_006E', 'B01001F_007E', 'B01001F_021E', 'B01001F_022E',
                 'B01001G_006E', 'B01001G_007E', 'B01001G_021E', 'B01001G_022E',
                 'B01001I_006E', 'B01001I_007E', 'B01001I_021E', 'B01001I_022E'
                 ]

    save_data(col_names, ','.join(var_names), 'acs_tracts_TN_2017.csv')


def get_data(region, headings, cols):

    url = 'https://api.census.gov/data/2017/acs/acs5'

    params = {
        'key': 'ab3870e2d20fc643fefe2e23361c8c2d40fbd0e1',
        'for': 'tract:*',
        'in': 'state:{}'.format(region),
        'get': cols
    }
    
    r = requests.get(url=url, params=params)

    data = json.loads(r.text)[1:]
    data.insert(0, headings)

    return data


def save_data(headings, cols, file):

    with open('./data/' + file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        data = get_data('47', headings, cols)  # TN
        writer.writerows(data)


if __name__ == '__main__':
    main()
