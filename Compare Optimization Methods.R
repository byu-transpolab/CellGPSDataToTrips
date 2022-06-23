sannboxparams <- read_csv("sannbox_params.csv")
optimparams <- read_csv("optim_params.csv")
optimerror <- read_csv("optim_error.csv")
sannboxerror <- read_csv("sannbox_error.csv")

cleanedParamValues <- function(file){
  paramsSep <- separate(file, col = x, 
                               into = "parameters", sep = "x") %>%
    filter(parameters != "")
  
  eps <- paramsSep[seq(1, nrow(paramsSep), 4), ] %>%
    as_tibble() %>%
    mutate(iterations = c(1:(nrow(paramsSep)/4)), eps = as.double(parameters)) %>%
    select(-c(parameters))
  minpts <- paramsSep[seq(2, nrow(paramsSep), 4), ] %>%
    as_tibble() %>%
    mutate(iterations = c(1:(nrow(paramsSep)/4)), minpts = as.double(parameters)) %>%
    select(-c(parameters))
  delta_t <- paramsSep[seq(3, nrow(paramsSep), 4), ] %>%
    as_tibble() %>%
    mutate(iterations = c(1:(nrow(paramsSep)/4)), delta_t = as.double(parameters)) %>%
    select(-c(parameters))
  entr_t <- paramsSep[seq(4, nrow(paramsSep), 4), ] %>%
    as_tibble() %>%
    mutate(iterations = c(1:(nrow(paramsSep)/4)), entr_t = as.double(parameters)) %>%
    select(-c(parameters))
  
  paramTable <- inner_join(eps,minpts,by = "iterations") %>%
    inner_join(delta_t, by = "iterations") %>%
    inner_join(entr_t, by = "iterations")
  
  paramTable <- paramTable %>%
    pivot_longer(
      cols = c(eps, minpts, delta_t, entr_t),
      names_to = "parameter"
    )
}

cleanedError <- function(file){
  errorSep <- separate(file, col = x, 
                        into = "error", sep = "x") %>%
    filter(error != "") %>%
    as_tibble() %>%
    mutate(iterations = c(1:nrow(errorSep))) %>%
    mutate(error = as.numeric(error))
}

optimError <- cleanedError(file = optimerror)
sannboxError <- cleanedError(file = sannboxerror)

sannboxTable <- cleanedParamValues(file = sannboxparams) %>%
  left_join(sannboxError, by = 'iterations')
optimTable <- cleanedParamValues(file = optimparams) %>%
  left_join(optimError, by = 'iterations')

ggplot(data = sannboxTable, aes(x = iterations, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_wrap(~parameter, scales = 'free') +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Sannbox Parameters")

ggplot(data = optimTable, aes(x = iterations, y = value)) +
  geom_line(aes(col = parameter)) +
  facet_wrap(~parameter, scales = 'free') +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Optim Parameters")

ggplot(data = sannboxTable %>%
         filter(parameter == "eps"),
       aes(x = iterations, y = as.numeric(error))) +
  geom_path() +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Sannbox Error")

sggplot(data = optimTable, aes(x = iterations, y = error)) +
  geom_line(aes(col = 'red')) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = "Optim Error")
