Rscript = /usr/local/bin/Rscript

data.out/%_democracy_scores_example.csv: data.in/democracies.csv
	$(Rscript) r/fit_democracy_model.R --model=$* --outfile=$@ --infile=$< --hyperparams=data.out/$*_expert_scores.csv --draws=2000 --warmup=1000 --chains=2 --example

data.out/%_democracy_scores.csv: data.in/democracies.csv
	$(Rscript) r/fit_democracy_model.R --model=$* --outfile=$@ --infile=$< --hyperparams=data.out/$*_expert_scores.csv --draws=2000 --warmup=1000 --chains=4

data.out/%_psrf.csv:
	$(Rscript) r/psrf.R --model=$* --outfile=$@

figs/%_expert_scores.pdf: data.out/%_expert_scores.csv
	$(Rscript) r/plot_experts.R --outfile=$@ --infile=$<

figs/expert_comparison.pdf: data.out/iid_expert_scores.csv data.out/iid_country_expert_scores.csv data.out/autocorr_expert_scores.csv
	$(Rscript) r/plot_experts_comparison.R --outfile=$@ --models=iid,autocorr

figs/%_democracy_scores.pdf: data.out/%_democracy_scores.csv
	$(Rscript) r/plot_democracies.R --outfile=$@ --infile=$< --countries=UKR,GRG,RUS,KYR,ARM,BLR,MLD --start-year=1990

figs/iid_country_params.pdf: data.in/democracies.csv
	$(Rscript) r/plot_iid_country_params.R --outfile=$@ --infile=$< --countries=UKR,GRG,RUS,KYR,ARM,BLR

figs/autocorr_params.pdf:
	$(Rscript) r/plot_autocorr_params.R --outfile=$@

all: figs/iid_democracy_scores.pdf figs/iid_country_democracy_scores.pdf figs/autocorr_democracy_scores.pdf figs/iid_expert_scores.pdf figs/iid_country_expert_scores.pdf figs/autocorr_expert_scores.pdf

clean:
	rm data.out/*.csv
	rm figs/*.{png,pdf}

deps:
	$(Rscript) r/install_stan.R
