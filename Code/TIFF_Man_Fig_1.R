# TIFF Figure 1 -----------------------------------------------------------

#### Figure 1


only_rho_03 = filter(skip_df, rho == 0.03)

load("../Generated_Data/Skip_Data/FromRepo_to_SN.Rdata")
head(FromRepo_to_SN)
FromRepo_to_SN_df = as.data.frame(FromRepo_to_SN)
FromRepo_to_SN_df$rho = FromRepo_to_SN_df$Rep/100
class(only_rho_03$rho)
class(FromRepo_to_SN_df$rho)
only_S_over_N_70 = join(only_rho_03, FromRepo_to_SN_df)

p = ggplot(data = only_S_over_N_70, aes(x = r0, y = Num_Skips,
                                        shape = delta)) +
  geom_point(size = 3) +
  labs(shape = expression(delta))+
  rahul_theme +
  theme_white_background + scale_shape_manual(values = c(18, 17, 15, 10,9,8,7,4,3), name = "")+
  labs(x = expression(R[0])) +
  labs(y = expression(paste("Number of skips (", n[c], ")"))) +
  rahul_man_figure_theme 
p


tiff(
  paste0(
    "../Figures/Manuscript_Figures/TIFF_Files/Fig2B.tiff"),
  height = 5, width = 10, res = 300, units = "in")
print(p)
dev.off()

only_delta_07 = filter(skip_df, delta == 0.7)
head(only_delta_07)
only_delta_07_S_over_N = join(only_delta_07, FromRepo_to_SN_df)
only_delta_07_S_over_N$FractSN = as.factor(as.character(
  only_delta_07_S_over_N$FractSN))
q = ggplot(data = only_delta_07_S_over_N, aes(x = r0, y = Num_Skips,
                                              shape = FractSN)) + geom_point(size = 3) +
  rahul_theme +
  labs(shape = expression(s[0]))+
  theme_white_background + scale_color_viridis_d(direction = -1)+
  labs(x = expression(R[0])) +
  scale_shape_manual(values = c(18, 17, 15), name = "") +
  labs(y = expression(paste("Number of skips (", n[c], ")"))) +
  rahul_man_figure_theme 
q


tiff(
  paste0(
    "../Figures/Manuscript_Figures/TIFF_Files/Fig2A.tiff"),
  height = 5, width = 10, res = 300, units = "in")
print(q)
dev.off()

p_comb = p
p_comb = p_comb 
q_comb = q
q_comb = q_comb 
q_comb = q_comb 
q_comb

p_comb = p_comb + 
  theme(legend.position = c(.60, .70))
q_comb = q_comb +
  theme(legend.position = c(.65, .75))
source("TIFF_Man_Fig_1_Panel_A.R")
#Fig_1_Panel_A

tiff(
  paste0(
    "../Figures/Manuscript_Figures/TIFF_Files/Fig1_raw.tiff"),
  height = 5, width = 10, res = 650, units = "in")
grid.arrange(Fig_1_Panel_A,p_comb, q_comb,
             ncol = 3)
dev.off()