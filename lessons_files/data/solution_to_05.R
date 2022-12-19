# load libraries
library("ggplot2")

# set path to data
path_to_data <- "/home/gabriele/work/microbiology/think_like_a_programmer/R_foundations/lessons_files/data/"

# load data
metadata <- read.csv(paste0(path_to_data, "metadata_final.csv"), header=T, sep=",")
counts <- read.csv(paste0(path_to_data, "counts_final.csv"), header=T, sep=",")

# get counts in a plottable format
# to do so, loop through the samples and
# get the abundances for each Phyla. In the
# meanwhile, store the info about which
# Phyla corresponds to that abundance
# the result will be a table with columns:
# sample | phyla | abundance | season | dna/rna | treatment

# define variables
which_phyla <- vector()
abundance <- vector()
season <- vector()
dna_rna <- vector()
treatment <- vector()
sample <- vector()

# loop through the samples in metadata
for (smpl in unique(metadata$sampleID)) {

	# loop through the Phyla
	for (phy in unique(counts$phylum)) {

		# store info about the phyla
		which_phyla <- append(which_phyla, phy)

		# find abundance
		abnd <- counts[which(counts$phylum==phy), which(colnames(counts)==smpl)]

		# store info about abundance, for (a_sample, a_phyla)
		abundance <- append(abundance, abnd)

		sample <- append(sample, smpl)
		season <- append(season, metadata$TimePoint[which(metadata$sampleID==smpl)])
		dna_rna <- append(dna_rna, metadata$DNA_RNA[which(metadata$sampleID==smpl)])
		treatment <- append(treatment, metadata$Treatment[which(metadata$sampleID==smpl)])
	}
}

# put everything together in as data trame
data_to_plot <- data.frame(sample, season, treatment, dna_rna, phylum=which_phyla, abundance)

# set levels for the variable season
data_to_plot$season <- factor(data_to_plot$season, levels=c("June", "July", "August"))

# set levels for the variable timepoint
metadata$TimePoint <- factor(metadata$TimePoint, levels=c("June", "July", "August"))
# set levels for the variable timepoint
metadata$Treatment <- factor(metadata$Treatment, levels=c("control", "warming"))

# define palette
palette <- c("lightcoral", "saddlebrown")
# define names of each colour to map species to colour
names(palette) <- levels(metadata$Treatment)

# define shapes
shapes <- c(21, 23, 25)
# define names of each colour to map species to colour
names(shapes) <- levels(metadata$TimePoint)

# loop through dna and rna
for (xna in unique(data_to_plot$dna_rna)) {

	xna_data <- data_to_plot[which(data_to_plot$dna_rna==xna), ]

	# for each phyla
	for (phy in unique(xna_data$phylum)) {

		# get the plot ready
		boxpl <- ggplot(xna_data[which(xna_data$phylum==phy), ], aes(x=treatment, y=abundance, fill=treatment)) +
				geom_boxplot() +
				facet_wrap(~season) +
				ggtitle(paste0(xna, " samples, in ", phy))

		# get a new window ready
		dev.new()
		# print the plot
		print(boxpl)
	}

	# scatterplot for enviro variables
	scatter <- ggplot(metadata[which(metadata$DNA_RNA==xna), ], aes(x=vegetation, y=pH, fill=Treatment, shape=TimePoint)) +
			geom_point(size=4, stroke=1, color="black") +
			scale_fill_manual(values=palette) +
			scale_shape_manual(values=shapes) +
			guides(fill=guide_legend(override.aes=list(shape=21))) +
			xlab("vegetation") +
			ylab("pH") +
			ggtitle(paste0(xna, " scatterplot")) +
			theme_bw() +
			theme(text=element_text(size=15, face="bold"), legend.position="right")

	# get a new window ready
	dev.new()
	# print the plot
	print(scatter)
	
}
