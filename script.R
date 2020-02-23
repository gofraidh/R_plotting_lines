
# Load data ---------------------------------------------------------------


mydata <- structure(list(variable = structure(c(2L, 1L, 1L, 1L, 1L, 1L, 
                                                3L, 1L, 1L, 1L, 5L, 1L, 1L, 6L, 1L, 1L, 1L, 4L, 1L, 1L), .Label = c("", 
                                                                                                                    "ethane", "ethene", "isobutane", "propane", "propene"), class = "factor"), 
                         time = c(2.067e-06, 0.0003354, 0.000668733, 0.001002067, 
                                  0.0013354, 0.001668733, 0.002002067, 0.0023354, 0.002668733, 
                                  0.003002067, 0.0033354, 0.003668733, 0.004002067, 0.0043354, 
                                  0.004668733, 0.005002067, 0.0053354, 0.005668733, 0.006002067, 
                                  0.0063354), peak_height = c(7.003645833, 7.006640625, 7.012239583, 
                                                              7.011328125, 7.008203125, 7.006510417, 7.011197917, 7.013411458, 
                                                              7.00859375, 7.005078125, 7.008984375, 7.016666667, 7.019401042, 
                                                              7.018098958, 7.013671875, 7.017838542, 7.026171875, 7.033072917, 
                                                              7.029557292, 7.021744792)), class = "data.frame", row.names = c(NA, 
                                                                                                                              -20L))


library(tidyverse)
library(ggrepel)
library(plotrix)
library(colorspace)


# Option 1 ----------------------------------------------------------------
# Ensure NA are labelled correctly; there is a possibility that the reason it takes so long
# is that since the variable is not NA but an empty string, instead of removing the row from labelling it, the ggrepel
# tries to plot empty string. I think it could be what you need while keeping the hack you did with labeling only specific points...
mydata %>% 
        mutate(variable = as.character(variable), # coerce to character
               variable = na_if(variable, ""), # change empty string to NA
               variable = factor(variable)) %>% # coerce back to factor
        ggplot(aes(time, peak_height)) + 
        geom_line() + 
        geom_label_repel(aes(label = variable), max.iter = 1000, seed = 23022020) +
        xlab("Retention Time (Minutes)") + 
        ylab("Peak Height") + 
        theme_minimal()


# Further options ---------------------------------------------------------
# Preprocess data to fill the empty strings
mydata_filled <- mydata %>% 
     mutate(variable = as.character(variable), # remove factor
            variable = na_if(variable, ""),
            variable = factor(variable)) %>% # repalce empty string
     fill(variable, .direction = c("down")) # fill NA with appropriate variables

glimpse(mydata_filled)

# Option 2, using central tendency to describe the data such as mean or median, and reducing iteration number in label_repel
mydata_filled %>% 
     group_by(variable) %>%
     summarise(time = mean(time), peak_height = mean(peak_height)) %>%
     ggplot(aes(time, peak_height)) + 
     geom_line() + 
     geom_label_repel(aes(label = variable), max.iter = 1000, seed = 23022020) + # Reduce iteration number to speed up a bit
     xlab("Retention Time (Minutes)") + 
     ylab("Peak Height") + 
     theme_minimal()

# Option 3, visualising this without relying on the geom_label, and using colors, linetype or pointtype.
ggplot(mydata_filled, aes(time, peak_height)) + 
        geom_point(aes(color = variable), alpha = 0.3, size = 4) +
        geom_smooth(se = FALSE, size = 0.5, method = "gam", formula = y ~ s(x, bs = "cs")) + 
        geom_step(aes(group = variable, color = variable), size = 0.8) +
        xlab("Retention Time (Minutes)") + 
        ylab("Peak Height") +
        theme_minimal()

# Option 4, using facets
ggplot(mydata_filled, aes(time, peak_height)) + 
        geom_step() + 
        # geom_label_repel(aes(label = variable)) +
        xlab("Retention Time (Minutes)") + 
        ylab("Peak Height") + 
        facet_grid(variable ~., scales = "free") +
        theme_minimal()

# Option 5, try filtering subset of data to label, thus minimising the number of variables
label_times_1 <- mydata_filled %>% 
        group_by(variable) %>%
        summarise(time = round(min(time), 7),
                  peak_height = round(mean(peak_height), 7))

label_times_2 <- mydata %>% 
        filter(variable != "") # data which is used only for labels...

ggplot(mydata_filled, aes(time, peak_height)) + 
        geom_line() + 
        geom_label_repel(data = label_times_2, aes(label = variable), max.iter = 1000, seed = 23022020) +
        xlab("Retention Time (Minutes)") + 
        ylab("Peak Height") + 
        theme_minimal()

# Comparison pot
ggplot(mydata, aes(time, peak_height)) + 
        geom_line() + 
        geom_label_repel(aes(label = variable), max.iter = 1000, seed = 23022020) +
        xlab("Retention Time (Minutes)") + 
        ylab("Peak Height") + 
        theme_minimal()


