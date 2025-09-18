import streamlit as st
import pandas as pd
import plotly.express as px

# -------------------------------
# Carregar dados
# -------------------------------
@st.cache_data
def load_data():
    df = pd.read_csv("dados_volumetria_cardapio.csv")
    # Remover coluna desnecessária
    if "Unnamed: 0" in df.columns:
        df = df.drop(columns=["Unnamed: 0"])
    return df

df = load_data()

st.title("🍽️ Análise de Volumetria x Cardápio")

# -------------------------------
# Filtros
# -------------------------------
st.sidebar.header("Filtros")

anos = st.sidebar.multiselect("Ano", df["Ano"].unique(), default=df["Ano"].unique())
meses = st.sidebar.multiselect("Mês", df["Mes"].unique(), default=df["Mes"].unique())
refeicoes = st.sidebar.multiselect("Refeição", df["refeicao"].dropna().unique(), 
                                   default=df["refeicao"].dropna().unique())

df_filtered = df[
    (df["Ano"].isin(anos)) &
    (df["Mes"].isin(meses)) &
    (df["refeicao"].isin(refeicoes))
]

# -------------------------------
# Visão Geral
# -------------------------------
st.subheader("📊 Volumetria ao longo do tempo")
fig = px.line(df_filtered, x="Data", y="Total_Dia", title="Total de refeições por dia")
st.plotly_chart(fig, use_container_width=True)

# -------------------------------
# Ranking por prato
# -------------------------------
st.subheader("🏆 Ranking de pratos")
ranking = (
    df_filtered.groupby("prato")["Total_Dia"]
    .mean()
    .sort_values(ascending=False)
    .reset_index()
    .head(15)
)

fig_ranking = px.bar(ranking, x="prato", y="Total_Dia",
                     title="Média de refeições por prato",
                     labels={"Total_Dia": "Média de Refeições", "prato": "Prato"})
fig_ranking.update_layout(xaxis_tickangle=-45)
st.plotly_chart(fig_ranking, use_container_width=True)

# -------------------------------
# Comparação Almoço x Jantar
# -------------------------------
st.subheader("🌞 Almoço x 🌙 Jantar")
comp = df_filtered.groupby("refeicao")[["Total_A", "Total_J"]].mean().reset_index()
fig_comp = px.bar(comp, x="refeicao", y=["Total_A", "Total_J"], barmode="group",
                  title="Média de refeições - Almoço vs Jantar")
st.plotly_chart(fig_comp, use_container_width=True)

# -------------------------------
# Detalhes por seleção
# -------------------------------
st.subheader("🔎 Detalhes por prato")
prato_sel = st.selectbox("Selecione um prato", df_filtered["prato"].dropna().unique())
df_prato = df_filtered[df_filtered["prato"] == prato_sel]

fig_prato = px.line(df_prato, x="Data", y="Total_Dia",
                    title=f"Refeições ao longo do tempo - {prato_sel}")
st.plotly_chart(fig_prato, use_container_width=True)
