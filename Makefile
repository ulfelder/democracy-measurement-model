data.out/%_democracy_scores.csv: data.in/democracies.csv
	Rscript r/fit_democracy_model.R --model=$* --outfile=$@ --infile=$< --hyperparams=data.out/$*_expert_scores.csv --draws=500

figs/%_expert_scores.pdf: data.out/%_expert_scores.csv
	Rscript r/plot_experts.R --outfile=$@ --infile=$<

figs/%_democracy_scores.pdf: data.out/%_democracy_scores.csv
	Rscript r/plot_democracies.R --outfile=$@ --infile=$< --countries=UKR,GRG,RUS,KYR,ARM,BLR

all: figs/iid_democracy_scores.pdf figs/autocorr_democracy_scores.pdf figs/iid_expert_scores.pdf figs/autocorr_expert_scores.pdf

clean:
	rm data.out/*.csv
	rm figs/*.{png,pdf}
