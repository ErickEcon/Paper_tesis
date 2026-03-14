import pandas as pd
import yfinance as yf
import datetime

def get_daily_prices(tickers, years_back):
    """
    Descarga la serie diaria de precios ajustados para uno o varios tickers de Yahoo Finance.
    
    Args:
        tickers (str or list): Símbolo del activo o lista de símbolos (ej: 'AAPL' o ['AAPL', 'MSFT']).
        years_back (int): Número de años de historia hacia atrás desde hoy.
        
    Returns:
        pd.DataFrame: DataFrame con los precios de cierre ajustados.
    """
    # Asegurar que tickers sea una lista para consistencia en yf.download
    if isinstance(tickers, str):
        tickers_list = [tickers]
    else:
        tickers_list = tickers
            
    # Calcular fechas de inicio y fin
    end_date = datetime.datetime.now()
    start_date = end_date - datetime.timedelta(days=years_back * 365)
    
    # Descargar datos
    # 'Adj Close' es preferible para análisis financiero ya que considera dividendos y splits
    df = yf.download(tickers_list, start=start_date, end=end_date)['Adj Close']
    
    # Manejo de la estructura de salida:
    # Se asegura que el resultado sea siempre un DataFrame, incluso para un solo ticker
    if isinstance(df, pd.Series):
        df = df.to_frame(name=tickers if isinstance(tickers, str) else tickers[0])
    
    return df

def calculate_log_returns(df):
    """
    Calcula los retornos logarítmicos a partir de un DataFrame de precios.
    
    Args:
        df (pd.DataFrame): DataFrame con series de precios en las columnas.
        
    Returns:
        pd.DataFrame: DataFrame con los retornos logarítmicos, sin valores nulos.
    """
    import numpy as np
    # Cálculo: ln(Pt / Pt-1)
    log_returns = np.log(df / df.shift(1)).dropna()
    
    return log_returns

def get_descriptive_stats(df):
    """
    Calcula estadísticos descriptivos y prueba de Dickey-Fuller para cada columna.
    
    Args:
        df (pd.DataFrame): DataFrame con series de retornos.
        
    Returns:
        pd.DataFrame: Resumen de estadísticos (Media, Min, Max, Std, Skew, Kurt, ADF p-value).
    """
    from statsmodels.tsa.stattools import adfuller
    import pandas as pd
    
    stats_list = []
    
    for col in df.columns:
        series = df[col].dropna()
        
        # Estadísticos básicos
        mean_val = series.mean()
        min_val = series.min()
        max_val = series.max()
        std_val = series.std()
        skew_val = series.skew()
        kurt_val = series.kurtosis() # Exceso de curtosis
        
        # Prueba de Dickey-Fuller Aumentada (ADF)
        adf_result = adfuller(series)
        adf_pvalue = adf_result[1]
        
        stats_list.append({
            'Ticker': col,
            'Media': mean_val,
            'Mínimo': min_val,
            'Máximo': max_val,
            'Desv. Est.': std_val,
            'Asimetría': skew_val,
            'Curtosis': kurt_val,
            'ADF p-value': adf_pvalue
        })
    
    stats_df = pd.DataFrame(stats_list).set_index('Ticker')
    return stats_df

def plot_price_returns_analysis(prices, returns, ticker_name):
    """
    Genera dos gráficos para el análisis de un activo:
    1. Precios y Retornos en el tiempo.
    2. Histograma de retornos con métricas de Asimetría y Curtosis.
    
    Args:
        prices (pd.Series or pd.DataFrame): Serie de precios.
        returns (pd.Series or pd.DataFrame): Serie de retornos.
        ticker_name (str): Nombre del ticker para el título.
    """
    import matplotlib.pyplot as plt
    import pandas as pd
    
    # Asegurar que sean Series para facilitar el manejo
    if isinstance(prices, pd.DataFrame): prices = prices.iloc[:, 0]
    if isinstance(returns, pd.DataFrame): returns = returns.iloc[:, 0]
    
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 10))
    plt.subplots_adjust(hspace=0.4)
    
    # --- Gráfico 1: Precios y Retornos ---
    ax1.plot(prices.index, prices, color='blue', label='Precio')
    ax1.set_ylabel('Precio', color='blue')
    ax1.tick_params(axis='y', labelcolor='blue')
    ax1.set_title(f'Análisis de Precios y Retornos: {ticker_name}')
    
    # Crear un segundo eje Y para los retornos
    ax1_ret = ax1.twinx()
    ax1_ret.plot(returns.index, returns, color='orange', alpha=0.5, label='Retornos')
    ax1_ret.set_ylabel('Retornos Logarítmicos', color='orange')
    ax1_ret.tick_params(axis='y', labelcolor='orange')
    
    ax1.grid(True, alpha=0.3)
    
    # --- Gráfico 2: Histograma con Asimetría y Curtosis ---
    ax2.hist(returns, bins=50, color='teal', alpha=0.7)
    ax2.set_title(f'Distribución de Retornos: {ticker_name}')
    ax2.set_xlabel('Retorno Logarítmico')
    
    # Calcular métricas para el texto
    skew_val = returns.skew()
    kurt_val = returns.kurtosis()
    
    stats_text = f'Asimetría: {skew_val:.2f}\nCurtosis: {kurt_val:.2f}'
    ax2.text(0.95, 0.95, stats_text, transform=ax2.transAxes, 
             verticalalignment='top', horizontalalignment='right',
             bbox=dict(boxstyle='round', facecolor='white', alpha=0.5))
    
    ax2.grid(True, alpha=0.3)
    
    plt.show()

def find_best_arima(series, max_p=3, max_d=1, max_q=3):
    """
    Realiza una búsqueda de malla (grid search) para encontrar los mejores parámetros ARIMA (p, d, q) 
    basados en el criterio de información de Akaike (AIC).
    
    Args:
        series (pd.Series): Serie de tiempo.
        max_p (int): Máximo orden de auto-regresión.
        max_d (int): Máximo orden de diferenciación.
        max_q (int): Máximo orden de media móvil.
        
    Returns:
        tuple: (best_order, best_aic, best_model)
    """
    from statsmodels.tsa.arima.model import ARIMA
    import warnings
    import itertools

    best_aic = float("inf")
    best_order = None
    best_mdl = None

    p_range = range(max_p + 1)
    d_range = range(max_d + 1)
    q_range = range(max_q + 1)
    pdq_combinations = list(itertools.product(p_range, d_range, q_range))

    with warnings.catch_warnings():
        warnings.filterwarnings("ignore")
        for order in pdq_combinations:
            try:
                tmp_mdl = ARIMA(series, order=order).fit()
                tmp_aic = tmp_mdl.aic
                if tmp_aic < best_aic:
                    best_aic = tmp_aic
                    best_order = order
                    best_mdl = tmp_mdl
            except:
                continue

    return best_order, best_aic, best_mdl

def analyze_autocorrelation(series, lags=20, ticker_name=""):
    """
    Realiza un análisis de autocorrelación incluyendo ACF, PACF y la prueba de Ljung-Box.
    
    Args:
        series (pd.Series): Serie de retornos.
        lags (int): Número de rezagos a analizar.
        ticker_name (str): Nombre del activo para los títulos.
        
    Returns:
        pd.DataFrame: Resultados de la prueba de Ljung-Box.
    """
    from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
    from statsmodels.stats.diagnostic import acorr_ljungbox
    import matplotlib.pyplot as plt
    
    series_clean = series.dropna()
    
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 10))
    plot_acf(series_clean, lags=lags, ax=ax1, title=f"ACF: Autocorrelación {ticker_name}")
    plot_pacf(series_clean, lags=lags, ax=ax2, title=f"PACF: Autocorrelación Parcial {ticker_name}")
    
    ax1.grid(True, alpha=0.3)
    ax2.grid(True, alpha=0.3)
    plt.tight_layout()
    plt.show()
    
    lb_results = acorr_ljungbox(series_clean, lags=[lags], return_df=True)
    return lb_results
