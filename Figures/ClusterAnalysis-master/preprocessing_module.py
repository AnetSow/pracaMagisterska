import pandas as pd
import numpy as np
from sklearn import preprocessing



def object_checking(df):
    """ Funkcja sprawdzająca czy w danych nie ma obiektów typu łańcuch znaków """
    object_value = df.select_dtypes(include='object')
    arr = np.array(object_value)

    if arr.dtype == 'object':
        print('Error: string-object in column: ' + str(object_value.columns[0])+'. Correct your data to a numeric type.')
    else:
        print('Correct data.')

# def comas_to_dots(df):
#     """ Funkcja konwertująca kropki na przecinki w liczbach zmiennoprzecinkowych """
#     df = pd.read_csv("C:/Users/Tomasz/Desktop/ClusterProject/iris_przecinki.csv", sep=";")
#     df.to_csv("iris_dot.csv", sep='\t', encoding='utf-8', decimal='.')


def header_checking(df):
    """ Funkcja sprawdzająca czy wszystkie kolumny mają nagłówki """

    headers = df.columns.str.contains('^Unnamed')

    if True in headers:
        print('Warning! Missing values in data. Please, complete headers in your data.')
    else:
        print('Your dataset is correct.')


def data_completeness(df):
    """ Funkcja sprawdzajająca czy wszystkie dane zostały uzupełnione """

    for key, value in df.iteritems():

        nan_value = value.hasnans
        if nan_value is True:
            print('Warning!', value.hasnans.sum(), 'uncompleted data at:\n', df[df.isnull().T.any().T])
        elif 'object' in df.dtypes:
            print('Warning! String object.')
        else:
            """" Eksport danych po standaryzacji do pliku (pamięci) wewnętrznego """
            df.to_csv('C:/Users/Tomasz/Desktop/ClusterProject/irisInner.csv', sep="\t", header=True)


def variable_renaming(df):
    """ Funkcja sprawadzająca czy poszczególne zmienne  mają swoje nazwy. Jeśli tak - przypisuje nazwy windeksom wierszy i usuwa pierwszą kolumną z nazwami. Jeśli nie - numeruje je kolejno zaczynając od 1. """
    print('Czy dane w poszczególnych wierszach zbioru danych zmienne mają swoje nazwy? [t/n] ')
    var_names = str(input())
    df.index.names = ['Name']
    if var_names.lower() == 't':
        df = df.rename(df.iloc[:, 0])
        df = df.drop(list(df)[0], axis=1)
        return df
    else:
        n = df.shape[0]
        df.index = range(1, n+1)
        return df


def standarize(df):
    x = df.iloc[:, 1:].values
    standscaler = preprocessing.StandardScaler()
    standscaler_df = standscaler.fit_transform(x)
    st_df = pd.DataFrame(standscaler_df)
    print("Standarized data: \n", st_df)
    print("Średnia: ", st_df.mean(axis=0))
    print("Odchylenie standardowe: \n", st_df.std(axis=0))

