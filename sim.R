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

# Generate the 320 tournament qualifiers
# Notice: No placeholder score/placement columns here to ensure a clean join later
qualifiers <- data.frame(
  wrestler_id = 1:(length(weight_classes) * num_wrestlers_per_weight),
  name = paste("Wrestler", 1:(length(weight_classes) * num_wrestlers_per_weight)),
  team = sample(teams, length(weight_classes) * num_wrestlers_per_weight, replace = TRUE),
  weight_class = rep(weight_classes, each = num_wrestlers_per_weight)
)

# ==========================================
# 2. MATCH AND BRACKET SIMULATION FUNCTIONS
# ==========================================

# Helper function to simulate a round of matches with equally likely bonus points
sim_matches <- function(wrestlers) {
  shuffled <- sample(wrestlers)
  n <- length(shuffled)
  
  # Pair them up: Evens win, Odds lose
  winners <- shuffled[seq(1, n, by = 2)]
  losers <- shuffled[seq(2, n, by = 2)]
  
  # Determine win type (25% chance for each)
  win_types <- sample(c("Dec", "MD", "TF", "Fall"), length(winners), replace = TRUE)
  
  # Assign bonus points based on NCAA rules
  bonus_points <- case_when(
    win_types == "MD" ~ 1.0,
    win_types == "TF" ~ 1.5,
    win_types == "Fall" ~ 2.0,
    TRUE ~ 0.0 # Regular Decision gets 0 bonus points
  )
  
  return(list(winners = winners, losers = losers, bonus_points = bonus_points))
}

# Function to simulate a full 32-man bracket
simulate_weight_class <- function(wrestler_ids) {
  scores <- setNames(rep(0, length(wrestler_ids)), wrestler_ids)
  placements <- setNames(rep("DNP", length(wrestler_ids)), wrestler_ids)
  
  # --- CHAMPIONSHIP ROUND 1 ---
  res <- sim_matches(wrestler_ids)
  champ_rd2 <- res$winners
  cons_rd1 <- res$losers
  scores[as.character(champ_rd2)] <- scores[as.character(champ_rd2)] + 1 + res$bonus_points
  
  # --- CHAMPIONSHIP ROUND 2 ---
  res <- sim_matches(champ_rd2)
  champ_qf <- res$winners
  cons_rd2_entrants <- res$losers
  scores[as.character(champ_qf)] <- scores[as.character(champ_qf)] + 1 + res$bonus_points
  
  # --- CONSOLATION ROUND 1 ---
  res <- sim_matches(cons_rd1)
  cons_rd2_adv <- res$winners
  scores[as.character(cons_rd2_adv)] <- scores[as.character(cons_rd2_adv)] + 0.5 + res$bonus_points
  
  # --- CONSOLATION ROUND 2 ---
  res <- sim_matches(c(cons_rd2_adv, cons_rd2_entrants))
  cons_rd3 <- res$winners
  scores[as.character(cons_rd3)] <- scores[as.character(cons_rd3)] + 0.5 + res$bonus_points
  
  # --- CHAMPIONSHIP QUARTERFINALS ---
  res <- sim_matches(champ_qf)
  champ_sf <- res$winners
  cons_rd4_entrants <- res$losers
  scores[as.character(champ_sf)] <- scores[as.character(champ_sf)] + 1 + res$bonus_points
  
  # --- CONSOLATION ROUND 3 ---
  res <- sim_matches(cons_rd3)
  cons_rd4_adv <- res$winners
  scores[as.character(cons_rd4_adv)] <- scores[as.character(cons_rd4_adv)] + 0.5 + res$bonus_points
  
  # --- CONSOLATION ROUND 4 (Blood Round - Winners are All-Americans) ---
  res <- sim_matches(c(cons_rd4_adv, cons_rd4_entrants))
  cons_qf <- res$winners
  scores[as.character(cons_qf)] <- scores[as.character(cons_qf)] + 0.5 + res$bonus_points
  
  # --- CHAMPIONSHIP SEMIFINALS ---
  res <- sim_matches(champ_sf)
  finals <- res$winners
  cons_sf_entrants <- res$losers
  scores[as.character(finals)] <- scores[as.character(finals)] + 1 + res$bonus_points
  
  # --- CONSOLATION QUARTERFINALS ---
  res <- sim_matches(cons_qf)
  cons_sf_adv <- res$winners
  seventh_place_match <- res$losers
  scores[as.character(cons_sf_adv)] <- scores[as.character(cons_sf_adv)] + 0.5 + res$bonus_points
  
  # --- CONSOLATION SEMIFINALS ---
  res <- sim_matches(c(cons_sf_adv, cons_sf_entrants))
  third_place_match <- res$winners
  fifth_place_match <- res$losers
  scores[as.character(third_place_match)] <- scores[as.character(third_place_match)] + 0.5 + res$bonus_points
  
  # --- PLACEMENT MATCHES (No advancement points awarded, but bonus points STILL apply) ---
  
  # 1st and 2nd Place
  res <- sim_matches(finals)
  first <- res$winners
  second <- res$losers
  scores[as.character(first)] <- scores[as.character(first)] + 16 + res$bonus_points
  scores[as.character(second)] <- scores[as.character(second)] + 12 
  placements[as.character(first)] <- "1st"
  placements[as.character(second)] <- "2nd"
  
  # 3rd and 4th Place
  res <- sim_matches(third_place_match)
  third <- res$winners
  fourth <- res$losers
  scores[as.character(third)] <- scores[as.character(third)] + 10 + res$bonus_points
  scores[as.character(fourth)] <- scores[as.character(fourth)] + 9
  placements[as.character(third)] <- "3rd"
  placements[as.character(fourth)] <- "4th"
  
  # 5th and 6th Place
  res <- sim_matches(fifth_place_match)
  fifth <- res$winners
  sixth <- res$losers
  scores[as.character(fifth)] <- scores[as.character(fifth)] + 7 + res$bonus_points
  scores[as.character(sixth)] <- scores[as.character(sixth)] + 6
  placements[as.character(fifth)] <- "5th"
  placements[as.character(sixth)] <- "6th"
  
  # 7th and 8th Place
  res <- sim_matches(seventh_place_match)
  seventh <- res$winners
  eighth <- res$losers
  scores[as.character(seventh)] <- scores[as.character(seventh)] + 4 + res$bonus_points
  scores[as.character(eighth)] <- scores[as.character(eighth)] + 3
  placements[as.character(seventh)] <- "7th"
  placements[as.character(eighth)] <- "8th"
  
  return(data.frame(
    wrestler_id = as.integer(names(scores)),
    points_earned = as.numeric(scores),
    placement = as.character(placements)
  ))
}

# ==========================================
# 3. RUN SIMULATION 
# ==========================================
results_list <- lapply(weight_classes, function(wc) {
  wc_wrestlers <- qualifiers %>% filter(weight_class == wc) %>% pull(wrestler_id)
  simulate_weight_class(wc_wrestlers)
})

# Combine results back into the main dataframe
tournament_results <- do.call(rbind, results_list)

# Join results to qualifiers (this is clean now because we didn't pre-create score/placement)
qualifiers <- qualifiers %>%
  left_join(tournament_results, by = "wrestler_id") %>%
  rename(score = points_earned)

# ==========================================
# 4. EXTRACT SCORES
# ==========================================

# INDIVIDUAL SCORES (Top 15 sorted by score)
individual_scores <- qualifiers %>%
  arrange(desc(score)) %>%
  select(name, team, weight_class, placement, score)

cat("\n--- TOP 15 INDIVIDUAL SCORES ---\n")
print(head(individual_scores, 15))

# TEAM SCORES (Sum of individual scores)
team_scores <- qualifiers %>%
  group_by(team) %>%
  summarise(
    total_score = sum(score),
    champions = sum(placement == "1st", na.rm = TRUE),
    all_americans = sum(placement %in% c("1st","2nd","3rd","4th","5th","6th","7th","8th"), na.rm = TRUE)
  ) %>%
  arrange(desc(total_score))

cat("\n--- TOP 10 TEAM SCORES ---\n")
print(head(team_scores, 10))

# ==========================================
# 5. SCORE INVERSION ANALYSIS
# ==========================================

# Create a numeric rank for easy comparison (DNP = 9)
qualifiers <- qualifiers %>%
  mutate(
    numeric_rank = case_when(
      placement == "1st" ~ 1,
      placement == "2nd" ~ 2,
      placement == "3rd" ~ 3,
      placement == "4th" ~ 4,
      placement == "5th" ~ 5,
      placement == "6th" ~ 6,
      placement == "7th" ~ 7,
      placement == "8th" ~ 8,
      TRUE ~ 9 
    )
  )

# Function to count inversions within a single weight class
count_inversions <- function(df) {
  n <- nrow(df)
  inversions <- 0
  valid_comparisons <- 0
  
  # Compare every wrestler against every other wrestler in the bracket
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      
      rank_i <- df$numeric_rank[i]
      rank_j <- df$numeric_rank[j]
      score_i <- df$score[i]
      score_j <- df$score[j]
      
      # We only care about comparing wrestlers who finished in DIFFERENT places
      if(rank_i != rank_j) {
        valid_comparisons <- valid_comparisons + 1
        
        # Check if the lower-placing wrestler has a STRICTLY HIGHER score
        if((rank_i > rank_j && score_i > score_j) || 
           (rank_j > rank_i && score_j > score_i)) {
          inversions <- inversions + 1
        }
      }
    }
  }
  return(data.frame(inversions = inversions, comparisons = valid_comparisons))
}

# Run the comparison across all 10 weight classes
inversion_results <- qualifiers %>%
  group_split(weight_class) %>%
  lapply(count_inversions) %>%
  bind_rows()

# Aggregate the totals
total_inversions <- sum(inversion_results$inversions)
total_comparisons <- sum(inversion_results$comparisons)
inversion_rate <- (total_inversions / total_comparisons) * 100

# Output the results
cat("\n--- SCORE INVERSION ANALYSIS ---\n")
cat(sprintf("Total Valid Pair Comparisons: %d\n", total_comparisons))
cat(sprintf("Times a Lower Placer Outscored a Higher Placer: %d\n", total_inversions))
cat(sprintf("Percentage of the time this happens: %.2f%%\n", inversion_rate))

# View specific examples of this happening
examples <- qualifiers %>%
  group_by(weight_class) %>%
  arrange(weight_class, numeric_rank) %>%
  inner_join(qualifiers, by = "weight_class", suffix = c("_high_placer", "_low_placer")) %>%
  filter(
    numeric_rank_high_placer < numeric_rank_low_placer, 
    score_high_placer < score_low_placer
  ) %>%
  select(
    weight_class,
    high_placer = name_high_placer,
    high_place = placement_high_placer,
    high_score = score_high_placer,
    low_placer = name_low_placer,
    low_place = placement_low_placer,
    low_score = score_low_placer
  )

cat("\n--- EXAMPLES OF LOWER PLACER OUTSCORING HIGHER PLACER ---\n")
print(head(examples, 10))