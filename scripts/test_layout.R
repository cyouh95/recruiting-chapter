library(igraph)


# Get ending coordinates
coords_kk <- layout_with_kk(g_1mode_pubu_psi)  # kk layout stays the same each time we run it
coords_fr <- layout_with_fr(g_1mode_pubu_psi)  # fr changes each time we run it

plot(g_1mode_pubu_psi, layout = layout_with_kk)
plot(g_1mode_pubu_psi, layout = coords_kk)  # equivalent

# Move x-coordinate for 100751 (originally at top right of plot) even more to the right
coords_kk[1, 1]
coords_kk[1, 1] <- 5

plot(g_1mode_pubu_psi, layout = layout_with_kk)  # original
plot(g_1mode_pubu_psi, layout = coords_kk)  # 100751's x-coordinate moved from 2.543199 to 5

# Specifying starting positions for vertices is option for layout_with_fr
# coords: Optional starting positions for the vertices. If this argument is not NULL then it should be an appropriate matrix of starting coordinates.
?layout_with_fr

# If we use layout_with_fr, the plot changes each time we run it
plot(g_1mode_pubu_psi, layout = layout_with_fr)

# Try plotting using coords_kk as starting coords 
# Plot still changes a lot each time we run it - hard to see effect of specifying starting coords
plot(g_1mode_pubu_psi, layout = layout_with_fr(g_1mode_pubu_psi, coords = coords_kk))

# Try testing different starting coords to see how it's affecting layout_with_fr
# Below coordinates will make vertices vertically aligned
starting_coords <- matrix(c(rep(0, 15), -5:9), ncol = 2)

# Test coordinates on layout_with_kk since it doesn't change each time
# Note that vertices are not evenly spaced along vertical line - suggesting kk layout moved it from this starting position
plot(g_1mode_pubu_psi, layout = layout_with_kk(g_1mode_pubu_psi, coords = starting_coords))

# Right now the script had coords specified for plot, which I don't think would have any effect
plot(g_1mode_pubu_psi, layout = layout_with_kk, coords = starting_coords)

# Test coordinates on layout_with_fr - results change every time
# Sometimes, we can see more clearly how vertices line up vertically, suggesting that starting coordinates was used, but the fr layout moved it a lot more afterwards
plot(g_1mode_pubu_psi, layout = layout_with_fr(g_1mode_pubu_psi, coords = starting_coords))

# Should still work if we save layout_with_fr to variable and call that, like how our functions are currently written
layout <- layout_with_fr
plot(g_1mode_pubu_psi, layout = layout(g_1mode_pubu_psi, coords = starting_coords))
