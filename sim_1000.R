# Load necessary library
library(dplyr)

# Set seed for reproducibility
set.seed(2026)

# ==========================================
# 1. SETUP TOURNAMENT PARAMETERS
# ==========================================
weight_classes <- c(125, 133, 141, 149, 157, 165, 174, 184, 197, 285)
num_wrestlers_per_weight <- 32

# Create a mock list of 50 NCAA Division 1 wrestling programs
teams <- paste("University", LETTERS, seq(1, 50))

# Generate the BASE qualifiers (metadata only, scores reset each run)
base_qualifiers <- data.frame(
  wrestler_id = 1:(length(weight_classes) * num_wrestlers_per_weight),
  name = paste("Wrestler", 1:(length(weight_classes) * num_wrestlers_per_weight)),
  team = sample(teams, length(weight_classes) * num_wrestlers_per_weight, replace = TRUE),
  weight_class = rep(weight_classes, each = num_wrestlers_per_weight)
)

# ==========================================
# 2. SIMULATION FUNCTIONS
# ==========================================
sim_matches <- function(wrestlers) {
  shuffled <- sample(wrestlers)
  n <- length(shuffled)
  
  winners <- shuffled[seq(1, n, by = 2)]
  losers <- shuffled[seq(2, n, by = 2)]
  
  # Determine win type (25% chance for each)
  win_types <- sample(c("Dec", "MD", "TF", "Fall"), length(winners), replace = TRUE)
  
  # Assign bonus points based on NCAA rules
  bonus_points <- case_when(
    win_types == "MD" ~ 1.0,
    win_types == "TF" ~ 1.5,
    win_types == "Fall" ~ 2.0,
    TRUE ~ 0.0
  )
  
  return(list(winners = winners, losers = losers, bonus_points = bonus_points))
}

simulate_weight_class <- function(wrestler_ids) {
  scores <- setNames(rep(0, length(wrestler_ids)), wrestler_ids)
  placements <- setNames(rep("DNP", length(wrestler_ids)), wrestler_ids)
  
  res <- sim_matches(wrestler_ids)
  champ_rd2 <- res$winners; cons_rd1 <- res$losers
  scores[as.character(champ_rd2)] <- scores[as.character(champ_rd2)] + 1 + res$bonus_points
  
  res <- sim_matches(champ_rd2)
  champ_qf <- res$winners; cons_rd2_entrants <- res$losers
  scores[as.character(champ_qf)] <- scores[as.character(champ_qf)] + 1 + res$bonus_points
  
  res <- sim_matches(cons_rd1)
  cons_rd2_adv <- res$winners
  scores[as.character(cons_rd2_adv)] <- scores[as.character(cons_rd2_adv)] + 0.5 + res$bonus_points
  
  res <- sim_matches(c(cons_rd2_adv, cons_rd2_entrants))
  cons_rd3 <- res$winners
  scores[as.character(cons_rd3)] <- scores[as.character(cons_rd3)] + 0.5 + res$bonus_points
  
  res <- sim_matches(champ_qf)
  champ_sf <- res$winners; cons_rd4_entrants <- res$losers
  scores[as.character(champ_sf)] <- scores[as.character(champ_sf)] + 1 + res$bonus_points
  
  res <- sim_matches(cons_rd3)
  cons_rd4_adv <- res$winners
  scores[as.character(cons_rd4_adv)] <- scores[as.character(cons_rd4_adv)] + 0.5 + res$bonus_points
  
  res <- sim_matches(c(cons_rd4_adv, cons_rd4_entrants))
  cons_qf <- res$winners
  scores[as.character(cons_qf)] <- scores[as.character(cons_qf)] + 0.5 + res$bonus_points
  
  res <- sim_matches(champ_sf)
  finals <- res$winners; cons_sf_entrants <- res$losers
  scores[as.character(finals)] <- scores[as.character(finals)] + 1 + res$bonus_points
  
  res <- sim_matches(cons_qf)
  cons_sf_adv <- res$winners; seventh_place_match <- res$losers
  scores[as.character(cons_sf_adv)] <- scores[as.character(cons_sf_adv)] + 0.5 + res$bonus_points
  
  res <- sim_matches(c(cons_sf_adv, cons_sf_entrants))
  third_place_match <- res$winners; fifth_place_match <- res$losers
  scores[as.character(third_place_match)] <- scores[as.character(third_place_match)] + 0.5 + res$bonus_points
  
  # PLACEMENT MATCHES
  res <- sim_matches(finals)
  scores[as.character(res$winners)] <- scores[as.character(res$winners)] + 16 + res$bonus_points
  scores[as.character(res$losers)] <- scores[as.character(res$losers)] + 12 
  placements[as.character(res$winners)] <- "1st"; placements[as.character(res$losers)] <- "2nd"
  
  res <- sim_matches(third_place_match)
  scores[as.character(res$winners)] <- scores[as.character(res$winners)] + 10 + res$bonus_points
  scores[as.character(res$losers)] <- scores[as.character(res$losers)] + 9
  placements[as.character(res$winners)] <- "3rd"; placements[as.character(res$losers)] <- "4th"
  
  res <- sim_matches(fifth_place_match)
  scores[as.character(res$winners)] <- scores[as.character(res$winners)] + 7 + res$bonus_points
  scores[as.character(res$losers)] <- scores[as.character(res$losers)] + 6
  placements[as.character(res$winners)] <- "5th"; placements[as.character(res$losers)] <- "6th"
  
  res <- sim_matches(seventh_place_match)
  scores[as.character(res$winners)] <- scores[as.character(res$winners)] + 4 + res$bonus_points
  scores[as.character(res$losers)] <- scores[as.character(res$losers)] + 3
  placements[as.character(res$winners)] <- "7th"; placements[as.character(res$losers)] <- "8th"
  
  return(data.frame(
    wrestler_id = as.integer(names(scores)),
    score = as.numeric(scores),
    placement = as.character(placements)
  ))
}

# Function to count inversions within a single weight class
count_inversions <- function(df) {
  n <- nrow(df)
  inversions <- 0
  valid_comparisons <- 0
  
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      if(df$numeric_rank[i] != df$numeric_rank[j]) {
        valid_comparisons <- valid_comparisons + 1
        if((df$numeric_rank[i] > df$numeric_rank[j] && df$score[i] > df$score[j]) || 
           (df$numeric_rank[j] > df$numeric_rank[i] && df$score[j] > df$score[i])) {
          inversions <- inversions + 1
        }
      }
    }
  }
  return(c(inversions = inversions, comparisons = valid_comparisons))
}

# ==========================================
# 3. WRAPPER FOR A SINGLE TOURNAMENT
# ==========================================
run_one_tournament <- function(sim_id) {
  
  # Run all weight classes
  results_list <- lapply(weight_classes, function(wc) {
    wc_wrestlers <- base_qualifiers %>% filter(weight_class == wc) %>% pull(wrestler_id)
    simulate_weight_class(wc_wrestlers)
  })
  
  tournament_results <- do.call(rbind, results_list)
  
  # Join and rank
  qualifiers <- base_qualifiers %>%
    left_join(tournament_results, by = "wrestler_id") %>%
    mutate(
      numeric_rank = case_when(
        placement == "1st" ~ 1, placement == "2nd" ~ 2, placement == "3rd" ~ 3,
        placement == "4th" ~ 4, placement == "5th" ~ 5, placement == "6th" ~ 6,
        placement == "7th" ~ 7, placement == "8th" ~ 8, TRUE ~ 9 
      )
    )
  
  # Calculate Score Inversions
  inv_metrics <- qualifiers %>%
    group_split(weight_class) %>%
    lapply(count_inversions)
  
  inv_matrix <- do.call(rbind, inv_metrics)
  total_inv <- sum(inv_matrix[, "inversions"])
  total_comp <- sum(inv_matrix[, "comparisons"])
  
  # Calculate Winning Team Score
  winning_score <- qualifiers %>%
    group_by(team) %>%
    summarise(total_score = sum(score)) %>%
    pull(total_score) %>%
    max()
  
  return(data.frame(
    sim_id = sim_id,
    inversions = total_inv,
    comparisons = total_comp,
    winning_team_score = winning_score
  ))
}

# ==========================================
# 4. RUN MONTE CARLO SIMULATION (1,000 Iterations)
# ==========================================
cat("Running 1,000 simulations. This may take 10-20 seconds...\n")

num_simulations <- 1000
simulation_results_list <- lapply(1:num_simulations, run_one_tournament)
all_simulations <- do.call(rbind, simulation_results_list)

# ==========================================
# 5. AGGREGATE AND PRINT FINAL METRICS
# ==========================================
grand_total_inversions <- sum(all_simulations$inversions)
grand_total_comparisons <- sum(all_simulations$comparisons)
expected_inversion_rate <- (grand_total_inversions / grand_total_comparisons) * 100

avg_winning_score <- mean(all_simulations$winning_team_score)

cat("\n==========================================\n")
cat("      1,000 TOURNAMENT SIMULATION RESULTS   \n")
cat("==========================================\n\n")

cat("--- SCORE INVERSION STATISTICS ---\n")
cat(sprintf("Total Valid Pair Comparisons across all runs: %s\n", format(grand_total_comparisons, big.mark=",")))
cat(sprintf("Total Times a Lower Placer Outscored a Higher Placer: %s\n", format(grand_total_inversions, big.mark=",")))
cat(sprintf("TRUE EXPECTED INVERSION RATE: %.2f%%\n", expected_inversion_rate))

cat("\n--- TEAM STATISTICS ---\n")
cat(sprintf("Average score required to win the Team National Title: %.1f points\n", avg_winning_score))
cat(sprintf("Highest winning score recorded in 1,000 runs: %.1f points\n", max(all_simulations$winning_team_score)))
cat(sprintf("Lowest winning score recorded in 1,000 runs: %.1f points\n", min(all_simulations$winning_team_score)))