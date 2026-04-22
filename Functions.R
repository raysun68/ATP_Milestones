library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

get_player <- function(df, player_name) {
  df %>%
    dplyr::filter(tolower(Name) == tolower(player_name))
}
classify_level <- function(p) {
  
  lvl10 <- any(!is.na(p$CGS))
  
  lvl9  <- any(!is.na(p$GSW) | !is.na(p$T1))
  
  lvl8  <- any(!is.na(p$GSF) | !is.na(p$YECW) | !is.na(p$OGG) | !is.na(p$T5))
  
  lvl7  <- any(!is.na(p$MT) | !is.na(p$GSSF) | !is.na(p$T10))
  
  lvl6  <- any(!is.na(p$MF) | !is.na(p$GSQF) | !is.na(p$OGM) | !is.na(p$YECF) | !is.na(p$T20))
  
  lvl5  <- any(!is.na(p$GSR4) | !is.na(p$T30))
  
  lvl4  <- any(!is.na(p$FT) | !is.na(p$T50))
  
  lvl3  <- any(!is.na(p$T100) | !is.na(p$FF))
  
  lvl2  <- any(!is.na(p$T200) | !is.na(p$CHT))
  
  lvl1  <- any(!is.na(p$T300) | !is.na(p$CHF))
  
  levels <- c(lvl10,lvl9,lvl8,lvl7,lvl6,lvl5,lvl4,lvl3,lvl2,lvl1)
  
  paste0("Level", 11 - which(levels)[1])
}
level_color <- function(level){
  
  lvl_num <- as.numeric(gsub("Level","",level))
  
  if(lvl_num == 10) return("rainbow")
  if(lvl_num <= 3) return("#CD7F32")   # bronze
  if(lvl_num <= 6) return("#C0C0C0")   # silver
  return("#FFD700")                    # gold
}
milestone_tier <- function(m){
  
  bronze <- c("T300","T200","T100","FF","CHT","CHF")
  silver <- c("T50","T30","T20","FT","MF","GSR4","GSQF","YECF","OGM")
  gold <- c("T10","T5","T1","MT","GSSF","GSF","GSW","YECW","OGG")
  black <- c("CH_Date")
  rainbow <- c("CGS")
  
  if(m %in% bronze) return("bronze")
  if(m %in% silver) return("silver")
  if(m %in% gold) return("gold")
  if(m %in% black) return("black")
  if(m %in% rainbow) return("rainbow")
  
  return("bronze")
}

tier_color <- function(t){
  
  if(t == "bronze") return("#CD7F32")
  if(t == "silver") return("#C0C0C0")
  if(t == "gold") return("#FFD700")
  if(t == "black") return("black")
  if(t == "rainbow") return("rainbow")
}

rainbow_cols <- function(n){
  grDevices::rainbow(n)
}

tier_height <- function(tier, max_other = 3) {
  if(tier == "bronze") return(0)
  if(tier == "silver") return(1)
  if(tier == "gold")   return(2)
  if(tier == "rainbow") return(3)
  if(tier == "black")  return(max_other)
}

build_timeline_age <- function(p_row) {
  ch_rank_val <- p_row$CH_Rank[1]
  p_row %>%
    select(ends_with("_age")) %>%
    pivot_longer(
      everything(),
      names_to = "milestone",
      values_to = "age"
    ) %>%
    mutate(
      milestone = str_remove(milestone, "_age"),
      CH_Rank = ch_rank_val   # add back player value
    ) %>%
    filter(!is.na(age)) %>%
    arrange(age) %>%
    mutate(
      tier = sapply(milestone, milestone_tier),    # assign tier per milestone
      base_col = sapply(tier, tier_color),        # color for the tier
      y = sapply(tier, function(t) tier_height(t))               # y base for the tier
    )
}

plot_timeline_age <- function(p_row, timeline_df, player_name, level) {
  level_num <- as.numeric(str_extract(level, "\\d+"))
  max_other = case_when(
    level_num %in% 1:3  ~ 0.6,
    level_num %in% 4:6  ~ 1.9,
    level_num %in% 7:10  ~ 2.9,
    TRUE            ~ 1.9  # default fallback
  )
  
  level_milestones <- list(
    `1` = c("T300", "CHF"),
    `2` = c("T200", "CHT"),
    `3` = c("T100", "FF"),
    `4` = c("T50", "FT"),
    `5` = c("T30", "GSR4"),
    `6` = c("T20", "MF", "GSQF"),
    `7` = c("T10", "MT", "GSSF"),
    `8` = c("T5", "GSF", "YECW"),
    `9` = c("GSW", "T1"),
    `10`= c("CGS")
  )
  
  # Get milestones for this level
  milestones_in_level <- level_milestones[[as.character(level_num)]]
  
  # Count which ones are completed
  completed <- sum(!is.na(p_row[milestones_in_level]))
  total <- length(milestones_in_level)
  
  # Build dot string: filled ● for completed, empty ○ for remaining
  filled_dot <- "\u25CF"  # black circle
  empty_dot  <- "\u25CB"  # white circle
  dots <- paste0(
    strrep(filled_dot, completed),
    strrep(empty_dot, total - completed)
  )
  title_text <- paste0(
    player_name,
    " (Lvl.", level_num, ") Career Milestones ",
    dots
  )
  
  
  timeline_df$plot_col <- timeline_df$base_col
  
  # Apply rainbow only to rainbow tier rows
  rainbow_idx <- timeline_df$tier == "rainbow"
  if (any(rainbow_idx)) {
    timeline_df$plot_col[rainbow_idx] <- rainbow(sum(rainbow_idx))
  }
  
  milestone_level_rank <- c(
    "CGS" = 10,
    "GSW" = 9, "T1" = 9,
    "GSF" = 8, "YECW" = 8, "OGG" = 8, "T5" = 8,
    "MT" = 7, "GSSF" = 7, "T10" = 7,
    "MF" = 6, "GSQF" = 6, "OGM" = 6, "YECF" = 6, "T20" = 6,
    "GSR4" = 5, "T30" = 5,
    "FT" = 4, "T50" = 4,
    "T100" = 3, "FF" = 3,
    "T200" = 2, "CHT" = 2,
    "T300" = 1, "CHF" = 1
  )
  
  
  # Replace milestone codes with readable labels
  timeline_df$milestone_rank <-
    milestone_level_rank[timeline_df$milestone]
  
  timeline_df <- timeline_df %>%
    mutate(
      milestone = case_when(
        milestone == "CHF" ~ "CH F",
        milestone == "CHT" ~ "CH W",
        milestone == "FT" ~ "Tour W",
        milestone == "FF" ~ "Tour F",
        milestone == "MT" ~ "MS W",
        milestone == "MF" ~ "MS F",
        milestone == "GSW" ~ "GS W",
        milestone == "GSF" ~ "GS F",
        milestone == "GSSF" ~ "GS SF",
        milestone == "GSQF" ~ "GS QF",
        milestone == "GSR4" ~ "GS R4",
        milestone == "YECW" ~ "YEC W",
        milestone == "YECF" ~ "YEC F",
        milestone == "OGM" ~ "OG Medal",
        milestone == "OGG" ~ "OG Gold",
        milestone == "CGS" ~ "Career Grand Slam / 10 Slams",
        milestone == "CH_Date" ~ paste0("Career High: #", CH_Rank),
        milestone == "T300" ~ "Top 300",
        milestone == "T200" ~ "Top 200",
        milestone == "T100" ~ "Top 100",
        milestone == "T50" ~ "Top 50",
        milestone == "T30" ~ "Top 30",
        milestone == "T20" ~ "Top 20",
        milestone == "T10" ~ "Top 10",
        milestone == "T5" ~ "Top 5",
        milestone == "T1" ~ "No.1",
        
        TRUE ~ milestone
      )
    )
  
  # Stack points **within each tier**
  timeline_df <- timeline_df %>%
    group_by(tier) %>%
    arrange(milestone_rank, age, .by_group = TRUE) %>%
    mutate(
      tier_base = tier_height(first(tier), max_other = max_other),        # base for this tier
      offset_idx = row_number() - 1,              # 0,1,2,...
      spacing = 0.1,                               # spacing between points
      y_stack = tier_base + offset_idx * spacing, # final stacked y
      text_y = y_stack - 0.02                      # text slightly below point
    ) %>%
    ungroup()
  
  # Compute x-axis limits
  min_age <- min(timeline_df$age, na.rm = TRUE)
  max_age <- max(timeline_df$age, na.rm = TRUE)
  
  # Ensure at least 5-year span
  if ((max_age - min_age) < 10) {
    mid_age <- (min_age + max_age) / 2
    min_age <- mid_age - 5
    max_age <- mid_age + 5
  }
  
  ggplot(timeline_df, aes(x = age, y = y_stack)) +
    
    # Regular points (everything except black and rainbow)
    geom_point(data = timeline_df %>% filter(!tier %in% c("black", "rainbow")),
               aes(color = plot_col),
               size = 3) +
    
    # Black tier as triangle
    geom_point(data = timeline_df %>% filter(tier == "black"),
               aes(color = plot_col),
               size = 3,
               shape = 17) +  # triangle
    
    geom_text(data = timeline_df %>% filter(tier == "rainbow"),
              aes(label = "\u2605", color = plot_col),
              size = 4,  # adjust size
              vjust = 0.5,
              hjust = 0.5) +   # star
    
    # Milestone labels
    geom_text(
      aes(x = age * 0.996, label = milestone, y = text_y),
      hjust = 1,
      fontface = "bold",
      size = 4
    ) +
    
    scale_color_identity() +
    coord_cartesian(xlim = c(min_age, max_age)) +  # <-- fixed x-axis limits
    
    labs(
      title = title_text,
      x = "Age",
      y = "Prestige"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 24), 
          axis.title.x = element_text(size = 24),      # x-axis title size
          axis.title.y = element_text(size = 24),      # y-axis title size
          axis.text.x  = element_text(size = 16, face = "bold"),  # x-axis ticks bold
          axis.text.y  = element_text(size = 12, face = "bold")       # y-axis ticks size
    )
}
player_career_plot_age <- function(df_age, player_name){
  
  p <- df_age %>%
    filter(tolower(Name) == tolower(player_name))
  
  if(nrow(p) == 0) stop("Player not found")
  
  level <- classify_level(p)   # your Step 4 function
  timeline <- build_timeline_age(p)
  plot_timeline_age(p, timeline, p$Name, level)
}

level_at_age <- function(player_i, age_A) {
  
  ages   <- age_mat[player_i, ]
  scores <- score_mat[player_i, ]
  
  # milestones that happened before age A
  valid <- which(!is.na(ages) & ages <= age_A)
  
  if (length(valid) == 0) {
    return(0)
  }
  
  max(scores[valid], na.rm = TRUE)
}

############################################################
# 6. Compute probability p for a given (A, l, L)
#
# p = proportion of training players with
#     level_score >= l by age A
#     among those whose final_score >= L
############################################################

compute_probability <- function(age_A, l, L) {
  
  # players whose final score >= L
  candidate_players <- train_idx[final_score[train_idx] >= L]
  
  if (length(candidate_players) == 0) {
    return(NA)
  }
  
  # check if they reached level >= l by age A
  reached <- sapply(candidate_players, function(i) {
    
    level_at_age(i, age_A) >= l
    
  })
  
  mean(reached)
}

stagnation_delta <- function(player_i, age_A, min_jump = 0.4) {
  
  ages   <- age_mat[player_i, ]
  scores <- score_mat[player_i, ]
  
  valid_idx <- which(!is.na(ages) & ages <= age_A)
  if(length(valid_idx) == 0) return(Inf)
  
  valid_scores <- scores[valid_idx]
  valid_ages   <- ages[valid_idx]
  
  non_na_idx <- which(!is.na(valid_scores))
  if(length(non_na_idx) == 0) return(Inf)
  
  valid_scores <- valid_scores[non_na_idx]
  valid_ages   <- valid_ages[non_na_idx]
  
  # initialize
  last_reset_age   <- valid_ages[1]
  last_reset_score <- valid_scores[1]
  
  if(length(valid_scores) > 1){
    
    for(i in 2:length(valid_scores)){
      
      # check improvement since last reset
      if(valid_scores[i] - last_reset_score >= min_jump){
        
        last_reset_age   <- valid_ages[i]
        last_reset_score <- valid_scores[i]
        
      }
      
    }
    
  }
  
  age_A - last_reset_age
}

find_L_values <- function(age_A, l, player_i) {
  if(l >= 10) return(list(L20=10, L50=10, L80=10))
  
  # compute thresholds based on training players
  L_grid <- seq(l, 10, by = 0.2)
  p_vals <- sapply(L_grid, function(L) compute_probability(age_A, l, L))
  
  find_cross <- function(threshold) {
    idx <- which(p_vals >= threshold)
    if(length(idx) == 0) return(NA)
    L_grid[min(idx)]
  }
  
  L20 <- find_cross(0.2)
  L50 <- find_cross(0.5)
  L80 <- find_cross(0.8)
  
  # Apply stagnation penalty
  delta_age = stagnation_delta(player_i, age_A, 0.4)
  
  if(delta_age >= 1.5 & !is.na(L50)){
    if(age_A <= 28){
      L50 <- l + max(0, (11.5 - delta_age) / 10) * (L50 - l)
      L80 <- l + max(0, (11.5 - delta_age) / 10) * max((L80 - l), 0.6)
    }
    else{
      L50 <- l + max(0, (6.5 - delta_age) / 5) * (L50 - l)
      L80 <- l + max(0, (6.5 - delta_age) / 5) * max((L80 - l), 0.2)
    }
  }
  if(delta_age < 1.5) {
    if(age_A <= 22){
      if(!is.na(L20)) L20 <- max(l + 0.1 * (1.5 - delta_age), L20)
      if(!is.na(L50)) L50 <- max(l + 0.2 * (1.5 - delta_age), L50)
    }
    else if(age_A <= 27){
      if(!is.na(L20)) L20 <- max(l + 0.1 * (1.5 - delta_age), L20)
      if(!is.na(L50)) L50 <- max(l + 0.5 * (1.5 - delta_age), L50)
      if(!is.na(L80)) L80 <- max(L80, l + 0.6 + 0.6 * (1.5 - delta_age))
      
    } else if(age_A < 30){
      
      # linear transition weight
      w <- (age_A - 27) / 3
      
      # blended coefficients
      L20_shift <- (1 - w) * (0.1 * (1.5 - delta_age))
      
      L50_shift <- (1 - w) * (0.5 * (1.5 - delta_age)) +
        w * (0.25 * (1.5 - delta_age))
      
      L80_shift <- (1 - w) * (0.6 + 0.6 * (1.5 - delta_age)) +
        w * (0.2 + 0.2 * (1.5 - delta_age))
      
      if(!is.na(L20)) L20 <- max(l + L20_shift, L20)
      if(!is.na(L50)) L50 <- max(l + L50_shift, L50)
      if(!is.na(L80)) L80 <- max(L80, l + L80_shift)
      
    } else {
      
      if(!is.na(L50)) L50 <- max(l + 0.25 * (1.5 - delta_age), L50)
      if(!is.na(L80)) L80 <- max(L80, l + 0.2 + 0.2 * (1.5 - delta_age))
      
    }
  }
  
  list(L20=L20, L50=L50, L80=L80)
}
############################################################
# 8. Build potential trajectory for a player
############################################################
potential_curve <- function(player_name) {
  player_i <- which(player_names == player_name)
  if(length(player_i) == 0) stop("Player not found")
  
  player_ages <- age_mat[player_i, ]
  start_age <- min(player_ages, na.rm = TRUE)
  end_age   <- min(max(max(player_ages, na.rm = TRUE) + 4, 30), age_prec$curr[player_i])
  age_grid <- seq(start_age, end_age, by = 0.1)
  
  current_level <- numeric(length(age_grid))
  L20 <- numeric(length(age_grid))
  L50 <- numeric(length(age_grid))
  L80 <- numeric(length(age_grid))
  
  for(g in seq_along(age_grid)) {
    A <- age_grid[g]
    l <- level_at_age(player_i, A)
    current_level[g] <- l
    
    L_vals <- find_L_values(A, l, player_i)
    L20[g] <- L_vals$L20
    L50[g] <- L_vals$L50
    L80[g] <- L_vals$L80
    
    # smooth transition weight
    w <- pmin(pmax((A - 17.5) / 3, 0), 1)
    
    # blended caps
    cap20 <- (1 - w) * 1.5 + w * 1
    cap50 <- (1 - w) * 3.5 + w * 2
    cap80 <- (1 - w) * 6.5 + w * 3
    
    # apply caps
    if(is.na(L20[g]) | L20[g] > min(l + cap20, 10)) L20[g] <- min(10, l + cap20)
    if(is.na(L50[g]) | L50[g] > min(l + cap50, 10)) L50[g] <- min(10, l + cap50)
    if(is.na(L80[g]) | L80[g] > min(l + cap80, 10)) L80[g] <- min(10, l + cap80)
    
  }
  
  data.frame(
    age = age_grid,
    current = current_level,
    p20 = L20,
    median = L50,
    p80 = L80
  )
}

############################################################
# 9. Plot function
plot_potential <- function(player_name) {
  df <- potential_curve(player_name)
  
  ggplot(df, aes(x = age)) +
    
    # Below median (floor / 20%)
    geom_ribbon(
      aes(ymin = p20, ymax = median, fill = "Floor(50-80%)"),
      alpha = 0.5
    ) +
    
    # Above median (ceiling / 80%)
    geom_ribbon(
      aes(ymin = median, ymax = p80, fill = "Ceiling(20-50%)"),
      alpha = 0.6
    ) +
    
    # Median line
    geom_line(
      aes(y = median, color = "Median"),
      linewidth = 1.5
    ) +
    
    # Current level (optional in legend or not)
    geom_line(
      aes(y = current, color = "Current"),
      linewidth = 1.5
    ) +
    
    # Colors
    scale_fill_manual(
      values = c(
        "Floor(50-80%)" = "#A6D8D4",
        "Ceiling(20-50%)"   = "#008080"
      )
    ) +
    
    scale_color_manual(
      values = c(
        "Median"  = "#006666",
        "Current" = "#222222"
      )
    ) +
    
    labs(
      title = paste("Potential Plot:", player_name),
      x = "Age",
      y = "Achievement Tier",
      fill = "",
      color = ""
    ) +
    
    scale_y_continuous(
      breaks = c(10, 8.2, 6.5, 4.8, 3.2, 1.6),
      labels = c("CGS", "GS Title", "Top 10", "Top 30", "Top 100", "Top 200")
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(face = "bold", size = 12),
      
      # Compact legend
      legend.position = "top",
      legend.direction = "horizontal",
      legend.text = element_text(size = 10),
      legend.key.size = unit(0.6, "lines"),
      legend.spacing.x = unit(0.3, "cm")
    )
}

trajectory_distance <- function(i, j, input_age, lambda = 0.5, alpha = 1){
  
  age_cols <- names(level_scores)[-1]
  
  ages_i <- as.numeric(age_prec[i, age_cols])
  scores_i <- as.numeric(level_scores[i, age_cols])
  
  ages_j <- as.numeric(age_prec[j, age_cols])
  scores_j <- as.numeric(level_scores[j, age_cols])
  
  df_i <- data.frame(age = ages_i, score = scores_i)
  df_j <- data.frame(age = ages_j, score = scores_j)
  
  df_i <- df_i[!is.na(df_i$age) & df_i$age <= input_age,]
  df_j <- df_j[!is.na(df_j$age) & df_j$age <= input_age,]
  
  df_i <- df_i[order(df_i$age),]
  df_j <- df_j[order(df_j$age),]
  
  breakpoints <- sort(unique(c(0, df_i$age, df_j$age, input_age)))
  
  si <- 0
  sj <- 0
  pi <- 1
  pj <- 1
  
  dist <- 0
  
  for(k in 1:(length(breakpoints)-1)){
    
    t0 <- breakpoints[k]
    t1 <- breakpoints[k+1]
    
    while(pi <= nrow(df_i) && df_i$age[pi] <= t0){
      si <- df_i$score[pi]
      pi <- pi + 1
    }
    
    while(pj <= nrow(df_j) && df_j$age[pj] <= t0){
      sj <- df_j$score[pj]
      pj <- pj + 1
    }
    
    c <- abs(si - sj)
    
    if(lambda == 0){
      
      dist <- dist + c * (t1 - t0)
      
    } else {
      
      dist <- dist + c *
        (exp(lambda*(t1-input_age)) -
           exp(lambda*(t0-input_age))) / lambda
    }
  }
  
  # terminal level difference penalty
  dist <- dist + alpha * abs(si - sj)
  
  return(dist)
}
find_similar_players <- function(player_name, input_age, lambda = 0.5){
  
  player_index <- which(age_prec$Name == player_name)
  
  if(length(player_index) == 0){
    stop("Player not found.")
  }
  
  distances <- numeric(nrow(age_prec))
  
  for(i in seq_len(nrow(age_prec))){
    
    if(i == player_index){
      distances[i] <- NA
    } else {
      distances[i] <- trajectory_distance(player_index, i, input_age, lambda)
    }
  }
  
  result <- data.frame(
    Name = age_prec$Name,
    Distance = distances,
    Birth = age_prec$Birth
  )
  
  cutoff_date <- as.Date("2026-06-01")
  
  result <- result[as.Date(result$Birth) + input_age*365.25 <= cutoff_date, ]
  
  result <- result[order(result$Distance),]
  
  head(result[!is.na(result$Distance), c("Name","Distance")], 10)
}

milestone_tier2 <- function(m){
  
  bronze <- c("T300_age","T200_age","T100_age","FF_age","CHT_age","CHF_age")
  silver <- c("T50_age","T30_age","T20_age","FT_age","MF_age","GSR4_age","GSQF_age","YECF_age","OGM_age")
  gold <- c("T10_age","T5_age","T1_age","MT_age","GSSF_age","GSF_age","GSW_age","YECW_age","OGG_age")
  rainbow <- c("CGS_age")
  
  if(m %in% bronze) return("bronze")
  if(m %in% silver) return("silver")
  if(m %in% gold) return("gold")
  if(m %in% rainbow) return("rainbow")
  
  return("bronze")
}

tier_color2 <- function(t){
  
  if(t == "bronze") return("#CD7F32")
  if(t == "silver") return("#C0C0C0")
  if(t == "gold") return("#FFD700")
  if(t == "rainbow") return("red")
}

plot_two_player_trajectories <- function(player1, player2, age_limit) {
  
  age_cols <- names(level_scores)[-1]
  
  prepare_player_df <- function(player_name) {
    
    player_age <- age_prec %>%
      filter(Name == player_name) %>%
      select(all_of(age_cols))
    
    player_score <- level_scores %>%
      filter(Name == player_name) %>%
      select(all_of(age_cols))
    
    curr <- age_prec %>%
      filter(Name == player_name) %>%
      pull(curr)
    
    df <- data.frame(
      Name = player_name,
      milestone = age_cols,
      age = as.numeric(player_age[1,]),
      score = as.numeric(player_score[1,])
    ) %>%
      filter(!is.na(age), age <= age_limit) %>%
      arrange(age)
    
    if(nrow(df) == 0) stop(paste(player_name, "has no milestones before this age."))
    
    last_age <- tail(df$age, 1)
    last_score <- tail(df$score, 1)
    
    # Solid trajectory (observed)
    traj_obs <- data.frame(
      Name = player_name,
      age = c(min(df$age), df$age, min(curr, age_limit)),
      score = c(0, df$score, last_score),
      linetype = "solid"
    )
    
    # Dashed projection (always from last milestone → age_limit)
    traj_proj <- NULL
    
    if(last_age < age_limit){
      traj_proj <- data.frame(
        Name = player_name,
        age = c(last_age, age_limit),
        score = c(last_score, last_score),
        linetype = "dashed"
      )
    }
    
    traj <- rbind(traj_obs, traj_proj)
    
    list(points = df, traj = traj)
  }
  
  p1 <- prepare_player_df(player1)
  p2 <- prepare_player_df(player2)
  
  traj_all <- rbind(p1$traj, p2$traj)
  points_all <- rbind(p1$points, p2$points)
  
  points_all$point_color <- sapply(points_all$milestone, function(m) {
    tier_color2(milestone_tier2(m))
  })
  
  line_colors <- setNames(c("#E69F9F", "#9FC0E6"), c(player1, player2))
  
  ggplot() +
    
    geom_step(
      data = traj_all,
      aes(x = age, y = score, color = Name, linetype = linetype),
      linewidth = 1.2
    ) +
    
    geom_point(
      data = points_all,
      aes(x = age, y = score, fill = point_color),
      shape = 21, size = 2, color = "black"
    ) +
    
    scale_color_manual(values = line_colors) +
    scale_fill_identity() +
    scale_linetype_identity() +
    
    labs(
      title = paste("Trajectory Comparison:", player1, "and", player2),
      x = "Age",
      y = "Prestige",
      color = "Player"
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.title.x = element_text(face = "bold", size = 16),
      axis.text.x = element_text(face = "bold", size = 16),
      axis.title.y = element_text(face = "bold", size = 16)
    )
}
