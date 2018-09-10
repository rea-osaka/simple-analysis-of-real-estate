#########   �n�斈�̎�����z�͈̔�  ##############
############  ������i���𗘗p�@################
#�\������
options(digits=10)
options(scipen=100)

# ������i���̑��݂���t�H���_�����āA�K�X�C��
setwd("C:/Users/_____/Desktop/torihiki")
library(tidyverse)

###�@�t�@�C�����̎擾 
fnames <- dir(pattern="^\\d{2}_.+_\\d{5}_\\d{5}.csv")  #�t�@�C�����̎擾
fnames <- fnames[c(27)]  # c( )���ɐ����@���s�ƕ���c(26, 28) �O�d�Ƒ��`�a�̎Rc(24,27:30)
fnames

### �t�@�C���̌���
dl <- lapply(fnames, read.csv,encoding = "Shift_JIS",na.strings = c("","NULL"))
data0 <- do.call(rbind, dl)
rm(dl)
data0 <- data0[, -1 ]
colnames(data0) <- c("���","�n��","�s�撬���R�[�h","�s���{����","�s�撬����","�n�於","�Ŋ�w����","�Ŋ�w��","������z","�ؒP��","�Ԏ��","�ʐ�","������i�̒P��","�y�n�̌`��","�Ԍ�","�����ʐ�","���z�N","�\��","�p�r","����","�O�ʓ��H�̕���","�O�ʓ��H�̎��","�O�ʓ��H����","�s�s�v��","������","�e�ϗ�","������_","����","���l")
data0$�s�n�� <- paste0(data0$�s�撬����, data0$�n�於)
data0$�w�� <- as.numeric(as.character( data0$�Ŋ�w��))
data0$�n�� <- as.numeric(as.character( data0$�ʐ�))
data0$����N <- as.integer(str_sub(as.character(data0$������_),3,4))+1988

### �i�荞��
data1 <- subset(data0, ��� =="��n(�y�n)" | ��� == "��n(�y�n�ƌ���)")
# �ȉ��A�i�荞�݂̗�B�R�����g��#���폜���ė��p���������B
# data1 <- subset(data1, �n�� == "�Z��n")
# data1 <- subset(data1, �n�� <= 400)
# data1 <- subset(data1, �Ŋ�w���� == "" | �Ŋ�w���� == "")
# data1 <- subset(data1, �Ŋ�w�� >= 5 & �Ŋ�w�� <= 15)
# data1 <- subset(data1, �s�撬���� == "�����s")
 
##### �n�斈�ɍŏ��`�ő�@
chiku_sogaku <- data1 %>% group_by(�s�n��) %>% summarise(
    n = n(), mean =mean(������z, na.rm= TRUE),
    min = min(������z, na.rm = TRUE ),
    p01 = quantile(������z, 0.01, na.rm = TRUE  ), p05 = quantile(������z, 0.05, na.rm = TRUE ),
    p10 = quantile(������z, 0.10, na.rm = TRUE ), p25 = quantile(������z, 0.25, na.rm = TRUE ),
    p50 = quantile(������z, 0.50, na.rm = TRUE ), p75 = quantile(������z, 0.75, na.rm = TRUE ),
    p90 = quantile(������z, 0.90, na.rm = TRUE ), p95 = quantile(������z, 0.95, na.rm = TRUE ),
    p99 = quantile(������z, 0.99, na.rm = TRUE ),
    max = max(������z, na.rm = TRUE)
    )
write.csv(chiku_sogaku, "chiku_sogaku.csv")    # setwd�̃t�H���_�ɕۑ�

###### �w���Ƃɍŏ��`�ő�
eki_sogaku <- group_by(data1, �Ŋ�w����) %>% summarise(
  n = n(), mean =mean(������z, na.rm= TRUE),
  min = min(������z, na.rm = TRUE ),
  p01 = quantile(������z, 0.01, na.rm = TRUE  ), p05 = quantile(������z, 0.05, na.rm = TRUE ),
  p10 = quantile(������z, 0.10, na.rm = TRUE ), p25 = quantile(������z, 0.25, na.rm = TRUE ),
  p50 = quantile(������z, 0.50, na.rm = TRUE ), p75 = quantile(������z, 0.75, na.rm = TRUE ),
  p90 = quantile(������z, 0.90, na.rm = TRUE ), p95 = quantile(������z, 0.95, na.rm = TRUE ),
  p99 = quantile(������z, 0.99, na.rm = TRUE ),
  max = max(������z, na.rm = TRUE)
)
write.csv(eki_sogaku, "eki_sogaku.csv")

############# ��@�����s

higashiosaka <- chiku_sogaku %>% subset(str_detect(�s�n��, "�����s")) %>% droplevels
# �T���v������10�N�]��10�������͖���
higashiosaka_over10 <- higashiosaka %>% subset(n >= 10)
# �ő�lmax�ŕ��ёւ�
higashiosaka_over10 <- higashiosaka_over10[order(higashiosaka_over10$max),]
# ���ʂƂ��āA�����̐���R�[���̑�n�̑��z���Ⴂ���Ƃ��m�F�ł���
# �t�@�C���ɕۑ�
write.csv(higashiosaka_over10, "higashiosaka_over10.csv")

## �O���t��
# ���z��������15�Ԗڂ܂ł̔��Ђ��}�I�ȃO���t
# �n�於�����o��
chiku15 <- higashiosaka_over10$�s�n��[1:15]
higashiosaka_chiku15 <- data1[data1$�s�n�� %in% chiku15,]
higashiosaka_chiku15$�s�n�� <- factor(higashiosaka_chiku15$�s�n��,levels = chiku15)
higashiosaka_chiku15 %>% ggplot(aes(�s�n��,������z)) + geom_boxplot(outlier.colour = NA) + geom_point(size=5, alpha=0.2)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
