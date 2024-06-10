# Análise de Conjuntura: Discriminação no Mercado de Trabalho Brasileiro
Repositório para análise conjuntural da discriminação racial no mercado de trabalho brasileiro


Considere a seguinte função de salários $\log(y_{iR}) = \alpha_i + X_i \beta + \gamma R_i + \varepsilon_i$, onde $R_i$ é a variável indicativa de raça negra. Nesse caso, salário esperado para brancos é $E(y_i | R = 0) = \alpha + E(X|R = 0) \beta$. Da mesma forma, salário esperado para negros é  $E(y_i | R = 1) = \alpha + E(X|R = 1) \beta + \gamma $.

Podemos escrever o salário médio de negros como: $E(y_i | R = 1) = \alpha + E(X|R = 1)\beta  + [E(X|R = 1) - E(X|R = 0) ]\beta + \gamma $. Ou seja
$E(y_i | R = 1) = E(y_i | R = 0) + [E(X|R = 1) - E(X|R = 0)]\beta + \gamma $. Defina $[E(X|R = 1) - E(X|R = 0)]$ como $\Delta X$.

A massa salarial em uma economia é dada por $\sum{y_i} = N \sum_{y_i}/N = N \bar{y}$. Seja a população dada por $N = N_0 + N_1$, então, $\sum{y_i} = N_0 \bar{y_0} +  N_1 \bar{y_1}$. Pelas formulas acima, $\sum{y_i} = N_0 E(y_i | R = 0) + N_1 [ E(y_i | R = 0) + \Delta X\beta + \gamma ] $. 

Ou ainda, $\sum{y_i} = N_0 \bar{y_0} + N_1 [\bar{y_0} + \Delta X\beta + \gamma ] = N \bar{y_0} + N_1 [\Delta X\beta + \gamma ]$.

**Massa salarial**: $N \bar{y_0} + N_1 [\Delta X\beta + \gamma ]$ 

Acima, estamos assumindo apenas uma penalidade salarial, mas não de emprego. Suponha que uma população seja de tamanho M, e que uma proporção p dela trabalhe. Logo, $N = pM$. Assumindo probabilidades de emprego diferentes para brancos e negros: $N = p_0M_0 + p_1M_1 = N_0 + N_1 $. Assumindo que a decisão de trabalhar não está associado ao salário potencial e que a probabilidade de trabalhar segue uma função linear (hipótese hercúlea). Podemos estimar o modelo de probabilidade linear: $Pr[p_i = 1] = \theta + Z_i\phi + R_i \delta + \omega_i$. Logo, a probabilidade de trabalhar para brancos e negros é respectivamente, $\bar{p}_0 = \theta + E(Z_i| R = 0) \phi $ 

e $\bar{p}_1 = \theta + E(Z_i| R = 1) \phi + \delta $, que é igual a $\bar{p}_0 + \Delta Z \phi + \delta $.

Então, $N_1 = [\bar{p}_0 + \Delta Z \phi + \delta] M_1  = [\bar{p}_0 M_1 ] + [\Delta Z \phi + \delta] M_1 $.

**Massa salarial perdida**: $N \bar{y_0} +  \bar{p}_0 M_1 [\Delta X\beta + \gamma ]  + M_1 [\Delta Z \phi + \delta]  [\Delta X\beta + \gamma ]$ 


e a massa salarial perdida em função da discriminação será:

$[\bar{p}_0 + \Delta Z \phi + \delta] M_1 \times [\Delta X\beta + \gamma ] $. Sem diferenças em covariadas: $M_1\times[\delta + \gamma(\delta+p_0)]$

Vale notar que $N = N_0 + N_1$, logo $N\bar{y_0}$ também é afetado por discriminação:
$N\bar{y_0} = \bar{y_0}[p_0M_0 + [\bar{p}_0 + \Delta Z \phi + \delta] M_1 ]$. 

Rearranjando: $\bar{y}_0 p_0 M + \bar{y_0} [\Delta Z \phi + \delta] M_1 $

Somando tudo, temos:
$\bar{y_0} [\Delta Z \phi + \delta] M_1 + [\bar{p}_0 + \Delta Z \phi + \delta] \times [\Delta X\beta + \gamma ] M_1 $

$[\bar{y_0} + \Delta X\beta + \gamma ] [\Delta Z \phi + \delta] M_1 + [\Delta X\beta + \gamma ] \bar{p}_0 M_1 $

$\bar{y_1} [\Delta Z \phi + \delta] M_1 + [\Delta X\beta + \gamma ] \bar{p}_0 M_1 $

O primeiro termo é o salário perdido devido a uma chance menor de conseguir emprego. O segundo termo é a massa salarial perdida devo ao fato de mesmo terem taxas de empregabilidade igual a de brancos, ainda há uma perda salarial devido a discrminação no mercado de trabalho.

Separando os termos $\Delta Z \phi$ e $\Delta X \beta$ de $\delta$ e $\gamma$, podemos ainda dividir essas perdas naquelas devidas as diferenças nas **características dos trabalhadores** e nas perdas devidas **puramente à discriminação**, tanto em termos de empregabilidade como em termos salariais.
