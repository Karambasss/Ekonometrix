import numpy as np
import statsmodels.api as sm
import statsmodels.stats as smd
import matplotlib.pyplot as plt
import statsmodels.stats.api as sms
from statsmodels.compat import lzip
from scipy.stats import t

data = np.genfromtxt(fname="data/task2.txt", delimiter="\t", skip_header=1)

print("Корреляционная матрица")
print(np.corrcoef(data.T))


def build_scatter(x, y, x_label: str, y_label: str, color: str = 'b'):
    plt.scatter(x=x, y=y, color=color)
    plt.xlabel(x_label)
    plt.ylabel(y_label)

    plt.show()


build_scatter(data[:, 1], data[:, 0], 'x1', 'y', 'r')
# build_scatter(data[:, 3], data[:, 0], 'x3', 'y', 'b')


def model_solve(y, x, name, alpha=0.05):
    print('*' * 25, name, '*' * 25)

    X = sm.add_constant(x)
    model = sm.OLS(y, X).fit()
    print(model.summary())
    print('Коэффициент аппроксимации =', np.mean(np.abs(model.resid / y)))

    k = x.shape[1] if len(x.shape) > 1 else 1

    t_crit = t.ppf(1 - alpha, len(y) - k - 1)
    print(f'Доверительные интервалы при alpha = {alpha}:')

    print(f'y: [{model.params[0] - model.bse[0] * t_crit / len(x) ** 0.5}; {model.params[0] + model.bse[0] * t_crit / len(x) ** 0.5}]')

    for i in range(k):
        if k != 1:
            x_cur = x[:, i]

        else:
            x_cur = x

        b = model.params[1 + i]
        print(f'x{1 + i}: [{b - model.bse[1 + i] * t_crit/ len(x) ** 0.5}; {b + model.bse[1 + i] * t_crit/ len(x) ** 0.5}]')
        beta = b * np.std(x_cur) / np.std(y)
        delta = np.corrcoef(x_cur, y)[0][1] * beta / model.rsquared
        elastic = b * x_cur.mean() / y.mean()

        print(f"Бета-коэффициент x{1 + i} =", beta)
        print(f"Дельта-коэффициент x{1 + i}=", delta)
        print(f"Коэффициент эластичности x{1 + i} =", elastic)

    # DWtest
    dw = smd.stattools.durbin_watson(model.resid)

    # # BGtest
    # smd.diagnostic.acorr_breusch_godfrey(model)
    # или
    name = ['Chisq statistic', 'p-value', 'F statistic', 'p-value(F)']
    bg = sms.acorr_breusch_godfrey(model, nlags=1)
    bg = lzip(name, bg)

    # GQtest
    name = ['F statistic', 'p-value']
    gq = sms.het_goldfeldquandt(model.resid, model.model.exog)
    gq = lzip(name, gq)
    # или GQtest
    # gq = sms.het_goldfeldquandt(model.resid, model.model.exog)

    # BPtest
    name = ['BP', 'p-value']
    bp = sms.het_breuschpagan(model.resid, model.model.exog)
    bp = lzip(name, bp)

    # White_test
    name = ['WT', 'p-value']
    wt = sms.het_white(model.resid, model.model.exog)
    wt = lzip(name, wt)

    print("Тест Дарбина-Уотсона:", dw)
    print("Тест Бреуша-Годфри:", bg)
    print("Тест Голдфельда-Квандта:", gq)
    print("Тест Бреуша-Пагана:", bp)
    print("Тест Уайта:", wt)
    return model


pair_model = model_solve(data[:, 0], data[:, 1], 'Парная модель', alpha=0.1)
# multiple_model = model_solve(data[:, 0], data[:, [1, 3]], 'Множественная модель')
#
# print('*' * 25, 'Тест на длинную и короткую регрессию', '*' * 25)
# ESS_R = sum(pair_model.resid ** 2)
# ESS_UR = sum(multiple_model.resid ** 2)
# q = 1
# n = data.shape[0]
# k = 2
# print('F-набл =', ((ESS_R - ESS_UR) / q) / (ESS_UR / (n - k - 1)))
