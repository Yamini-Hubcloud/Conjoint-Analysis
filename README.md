# Conjoint-Analysis

This project applies conjoint analysis to evaluate consumer preferences for car attributes, aiming to inform data-driven product development and marketing strategies. The analysis was conducted in R using the conjoint package, focusing on key automotive features: Price, Brand, Horsepower, Color, and Sunroof.

A fractional factorial design was employed to reduce the number of product profiles from all possible combinations to a manageable and statistically valid set of 25, ensuring efficient data collection while maintaining the ability to estimate part-worth utilities. Correlation checks ensured the design’s orthogonality, preventing multicollinearity between attribute levels.

Respondents' preference rankings were collected and analyzed to estimate part-worth utilities for each level of every attribute. These values quantify the influence of each feature on consumer decision-making. To enhance interpretability, part-worths were normalized by setting a baseline for each attribute (e.g., lowest price, least horsepower).

The model outputs were further used to derive key marketing insights:

Utility Scores indicate feature desirability and trade-offs.

Logit Share of Preference models market share estimation under different product scenarios.

First Choice Model helps identify the most preferred product configurations.

Willingness To Pay (WTP) was inferred to estimate how much extra consumers would pay for specific features.

The resulting insights were visualized in Tableau through bar charts, line plots, and heatmaps, highlighting consumer priorities and supporting strategic decisions in product design, pricing, and segmentation.

Overall, this conjoint analysis provides a robust, quantitative framework to simulate market reactions to product changes and optimize value propositions for target segments.
