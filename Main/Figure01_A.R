library(tidyverse)
library(ggnewscale)
library(patchwork)

### Input data preparation ----------------------------------------------------------------------------------------
data_path <- "https://raw.githubusercontent.com/pughlab/IRIS_TCR/refs/heads/main/Data"

### Clinical data:
clinical_data_fname <- "02_IRIS_ClinicalData.csv"

clinical_data <- readr::read_csv(
        file.path (data_path , clinical_data_fname))

### Sample inventory and technologies:
sample_inventory_fname <- "01_IRIS_SampleInventory.csv"

sample_inventory <- readr::read_csv(
        file.path (data_path , sample_inventory_fname))%>%
        mutate(`Sample type` = fct_relevel(`Sample type` , 
                                           "Tumour" , "Buffy coat" , "Plasma cfDNA") ,
               Technology = fct_relevel(Technology ,
                                        "WGTA" ,  "IHC" , "Bulk CapTCR-seq" , "scRNA-seq" , "CITESeq"))

### Color Palettes ------------------------------------------------------------------------------------------------

gender_ColPal <- tibble(gender = c("Female" , "Male") ,
                        color = c("#000000" , "#989796"))

treatment_ColPal <- tibble(therapy = c("immunotherapy + other treatment" ,
                                      "immunotherapy combination" ,
                                      "immunotherapy single agent") ,
                           color = c("#654023" , "#BB9E3F" , "#DED6C5"))

cancer_ColPal <- tibble(ONCOTREE_main_type = c("Melanoma" ,
                                               "Head and Neck Cancer" ,
                                               "Colorectal Cancer" ,
                                               "Endometrial Cancer" ,
                                               "Mesothelioma" ,
                                               "Skin Cancer, Non-Melanoma" ,
                                               "Salivary Gland Cancer" ,
                                               "Esophagogastric Cancer") ,
                           color = c("#09522B" ,
                                     "#8EAE29" ,
                                     "#D3E0C1" ,
                                     "#FFFBB3" ,
                                     "#FFC72C" ,
                                     "#DC6602" ,
                                     "#AA2600" ,
                                     "#0059B3"))

### Defining the order of patients on y-axis ----------------------------------------------------------------------

PatientOrder <- (sample_inventory %>%
        mutate(Count = 1) %>%
        pivot_wider(names_from = c(Technology , `Sample type`)  ,
                    values_from = Count) %>%
                mutate(TechCount = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
        arrange(desc (TechCount) ,
                desc(WGTA_Tumour) ,
                desc(IHC_Tumour) ,
                desc (`Bulk CapTCR-seq_Tumour`) ,
                desc (`scRNA-seq_Tumour`) ,
                desc (CITESeq_Tumour)))$Patient_id

### Plot Vis; Horizontal Version ----------------------------------------------------------------------------------

Technologies_Horizontal <- 
ggplot(data = sample_inventory %>%
                               mutate(Patient_id = fct_relevel(Patient_id , PatientOrder))%>%
               mutate(
                      Technology = fct_relevel(Technology ,
                                               rev (c ("WGTA" ,  "IHC" , "Bulk CapTCR-seq" , "scRNA-seq" , "CITESeq")) ))) +
        geom_tile(
                aes(
                        y =  Technology,
                        x = Patient_id ,
                        #fill = `Oncotree type`
                ),
                color = "#000000" ,
                fill = "#000000" ,
                linewidth = 0.05 ,
                width = 0.85 , height = 0.85 )+
        
        scale_x_discrete() +
        facet_grid(`Sample type` ~ `Resistance`,
                   scales = "free" ,
                   space = "free" ,
                   labeller = as_labeller(c(`Tumour` = "Tumour" ,
                                            `Buffy coat` = "Buffy\ncoat" ,
                                            `Plasma cfDNA` = "Plasma\ncfDNA" ,
                                            `Acquired resistance` = "Acquired\nresistance" ,
                                            `Primary resistance` = "Primary\nresistance")) ,
                   switch = "x") +
        scale_y_discrete(  #guide = guide_axis(n.dodge = 1),
                expand = c(0.3 , 0.3)) +
        theme_minimal() +
        theme(
                panel.grid = element_blank() ,
                panel.spacing.x = unit(1.5 , units = "line"),
                panel.spacing.y = unit(0.5 , units = "line"),
                axis.ticks.y = element_line(linewidth = 0.25 , color = "#000000"),
                axis.line = element_line(color = "#000000" , linewidth = 0.25) ,
                
                
                axis.title = element_blank() ,
                
                axis.text.x = element_blank() ,
                axis.text.y = element_text(size = 8 ,  color = "#000000") ,
                
                strip.text.x.bottom = element_text(size = 8 , color = "#000000") ,
                strip.text.y = element_text(size = 8 , color = "#000000" , angle = 0))




MetaData_Horizontal <- 
        ggplot(data = clinical_data %>%
                           mutate(Patient_id = fct_relevel(Patient_id , PatientOrder)) ) +
        geom_tile(
                aes(
                        y = "Gender" ,
                        x = Patient_id ,
                        fill = Gender
                ),
                color = "#000000" ,
                linewidth = 0.05 ,
                width = 0.9 , height = 0.9 )+
        scale_fill_manual(breaks = gender_ColPal$gender ,
                          values = gender_ColPal$color) +
        
        #--------------------------------------------------------------------------
        ggnewscale::new_scale_fill() +
        geom_tile(
                aes(
                        y = "Treatment" ,
                        x = Patient_id ,
                        fill = `Treatment class`
                ),
                color = "#000000" ,
                linewidth = 0.05 ,
                width = 0.9 , height = 0.9 )+
        scale_fill_manual(breaks = treatment_ColPal$therapy ,
                          values = treatment_ColPal$color) +
        
        #--------------------------------------------------------------------------
        ggnewscale::new_scale_fill() +
        
        geom_tile(
                aes(
                        y = "Cancer type" ,
                        x = Patient_id ,
                        fill = `ONCOTREE main type`
                ),
                color = "#000000" ,
                linewidth = 0.05 ,
                width = 0.9 , height = 0.9 )+
        scale_fill_manual(breaks = cancer_ColPal$ONCOTREE_main_type ,
                          values = cancer_ColPal$color) +
        
        
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = c("Gender" , "Cancer type", "Treatment")) +
        facet_grid(. ~ `Resistance`,
                   scales = "free" ,
                   space = "free" ,
                   labeller = as_labeller(c(
                           `Acquired resistance` = "Acquired\nresistance" ,
                           `Primary resistance` = "Primary\nresistance"))) +
        theme_minimal() +
        theme(
                panel.grid = element_blank() ,
                panel.spacing.y = unit(0.5 , units = "line"),
                axis.ticks = element_blank(),
                axis.line = element_blank() ,
                
                
                axis.title = element_blank() ,
                
                axis.text.y = element_text(size = 8 , color = "#000000" , vjust = 0) ,
                axis.text.x.top = element_text(size = 8 ,  color = "#000000" , angle = 90 , vjust = 0.5) ,
                
                strip.text.x = element_blank() ,
                strip.text.y = element_blank() ,
                
                legend.text = element_text(size = 8 , color = "#000000") ,
                legend.title = element_text(size = 8 , color = "#000000") )


Design <- "A
B"


ggsave(
        file.path(
                "/Users/shirin/Desktop/Immunarch/downsamplign/Projetcs",
                "IRIS/Sample_Archive" ,
                "IOKIN_SampleInventory-and-Technologies_HorizontalVersion.svg" ),
        device = "svg" ,
        height = 1500,
        width = 2300,
        units = "px",
        bg = "transparent",
        dpi = 320)

MetaData_Horizontal + Technologies_Horizontal + patchwork::plot_layout(design = Design ,
                                                 guides='collect' ,
                                                 heights = c(0.26 , 1.5)) &
        theme(legend.position = "bottom" ,
              legend.direction = 'horizontal')

dev.off ()

### Plot Vis; Vertical Version ------------------------------------------------------------------------------------

Technologies <- ggplot(data = sample_inventory %>%
               mutate(Patient_id = fct_relevel(Patient_id , PatientOrder)) ) +
        geom_tile(
                aes(
                        x =  Technology,
                        y = Patient_id ,
                        #fill = `Oncotree type`
                ),
                color = "#000000" ,
                fill = "#000000" ,
                linewidth = 0.05 ,
                width = 0.85 , height = 0.85 )+
        
        scale_y_discrete(limits = rev) +
        facet_grid(`Resistance` ~ `Sample type`,
                   scales = "free" ,
                   space = "free" ,
                   labeller = as_labeller(c(`Tumour` = "Tumour" ,
                                            `Buffy coat` = "Buffy\ncoat" ,
                                            `Plasma cfDNA` = "Plasma\ncfDNA" ,
                                            `Acquired resistance` = "Acquired\nresistance" ,
                                            `Primary resistance` = "Primary\nresistance"))) +
        scale_x_discrete(  #guide = guide_axis(n.dodge = 1),
                           expand = c(0.3 , 0.3)) +
        theme_minimal() +
        theme(
                panel.grid = element_blank() ,
                panel.spacing.x = unit(0.5 , units = "line"),
                panel.spacing.y = unit(1.5 , units = "line"),
                axis.ticks.x = element_line(linewidth = 0.25 , color = "#000000"),
                axis.line.x = element_line(color = "#000000" , linewidth = 0.25) ,
                
                
                axis.title = element_blank() ,
                
                #axis.text.y = element_text(size = 8 , color = "#000000") ,
                axis.text.y = element_blank() ,
                axis.text.x = element_text(size = 8 ,  color = "#000000" , angle = 30 , hjust = 1 ) ,
                
                strip.text.x = element_text(size = 8 , color = "#000000") ,
                strip.text.y = element_text(size = 8 , color = "#000000" , angle = 0))




MetaData <- ggplot(data = clinical_data %>%
               mutate(Patient_id = fct_relevel(Patient_id , PatientOrder)) ) +
        geom_tile(
                aes(
                        x = "Gender" ,
                        y = Patient_id ,
                        fill = Gender
                ),
                color = "#000000" ,
                linewidth = 0.05 ,
                width = 0.9 , height = 0.9 )+
        scale_fill_manual(breaks = gender_ColPal$gender ,
                          values = gender_ColPal$color) +
        
        #--------------------------------------------------------------------------
        ggnewscale::new_scale_fill() +
        geom_tile(
                aes(
                        x = "Treatment" ,
                        y = Patient_id ,
                        fill = `Treatment class`
                ),
                color = "#000000" ,
                linewidth = 0.05 ,
                width = 0.9 , height = 0.9 )+
        scale_fill_manual(breaks = treatment_ColPal$therapy ,
                          values = treatment_ColPal$color) +
        
        #--------------------------------------------------------------------------
        ggnewscale::new_scale_fill() +
        
        geom_tile(
                aes(
                        x = "Cancer type" ,
                        y = Patient_id ,
                        fill = `ONCOTREE main type`
                ),
                color = "#000000" ,
                linewidth = 0.05 ,
                width = 0.9 , height = 0.9 )+
        scale_fill_manual(breaks = cancer_ColPal$ONCOTREE_main_type ,
                          values = cancer_ColPal$color) +
        
        
        scale_y_discrete(limits = rev) +
        scale_x_discrete(limits = c("Treatment", "Cancer type", "Gender")) +
        facet_grid(`Resistance` ~ .,
                   scales = "free" ,
                   space = "free" ,
                   labeller = as_labeller(c(
                                            `Acquired resistance` = "Acquired\nresistance" ,
                                            `Primary resistance` = "Primary\nresistance"))) +
        theme_minimal() +
        theme(
                panel.grid = element_blank() ,
                panel.spacing.y = unit(1.5 , units = "line"),
                axis.ticks = element_blank(),
                axis.line = element_blank() ,
                
                
                axis.title = element_blank() ,
                
                axis.text.y = element_text(size = 8 , color = "#000000") ,
                axis.text.x = element_text(size = 8 ,  color = "#000000" , angle = 30 , hjust = 1) ,
                
                strip.text.x = element_text(size = 8 , color = "#000000") ,
                strip.text.y = element_blank() ,
                
                legend.text = element_text(size = 8 , color = "#000000") ,
                legend.title = element_text(size = 8 , color = "#000000") )


Design <- "AB"


ggsave(
        file.path(
                "/Users/shirin/Desktop/Immunarch/downsamplign/Projetcs",
                "IRIS/Sample_Archive" ,
                "IOKIN_SampleInventory-and-Technologies.svg" ),
        device = "svg" ,
        height = 2250,
        width = 2200,
        units = "px",
        bg = "transparent",
        dpi = 320)

MetaData + Technologies + patchwork::plot_layout(design = Design ,
                                                 guides='collect' ,
                                                 widths = c(0.25 , 1.5)) &
        theme(legend.position = "right" ,
              legend.direction = 'vertical')

dev.off ()
