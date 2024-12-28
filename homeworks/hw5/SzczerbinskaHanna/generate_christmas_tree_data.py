import math
import pandas as pd


def generate_chebyshev_nodes(num_nodes):
    node_list = [0] * num_nodes
    for k in range(num_nodes):
        node_list[k] = math.cos(math.pi * (2*k + 1) / (2*num_nodes))
    return node_list


def generate_full_data(node_list, height=4):
    n = len(node_list)

    name_list = [0] * (n + 1)
    name_list[0] = 'y'
    for i in range(n):
        name_list[i+1] = 'x' + str(i+1)

    full_data = [[0] * (n+1) for i in range(2)]
    full_data[1][0] = height
    for i in range(len(node_list)):
        full_data[0][i+1] = node_list[i]

    df = pd.DataFrame(full_data, columns=name_list)
    return df


def save_into_csv(df):
    df.to_csv("full_data.csv")


if __name__ == '__main__':
    save_into_csv(generate_full_data(generate_chebyshev_nodes(50)))
