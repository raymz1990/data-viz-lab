# Personal Data Visualization Projects

This repository is dedicated to sharing my journey in exploring *data visualization* and analysis with `R`.

The directories will include datasets and code files. Code files will include comments to walk through different steps of generating the visualiztion.

---

## **Summary**

| **Month** | **Topic**                               | **Main Packages**               | **Source**      |
|:---------:|:----------------------------------------|:--------------------------------|:----------------|
| 01        | [Formula One](./2025/month-01)          | ggplot, ggtext, ggimage, ggbump | Wikipedia       |

---

## **Projects**

### **[P01 - The Formula One Legacy of Ayrton Senna](./2025/month-01/p01-senna_carrer)**
Inspired by Tanya Shapiro's [Leo's Girlfriend Chart](https://github.com/tashapiro/tanya-data-viz/blob/main/dicaprio-gfs/dicaprio-gfs.R), this visualization explores Ayrton Senna's career in Formula One. Each car image represents the corresponding season.

The plot was created using `ggplot`, `ggtext`, and `ggimage`.

![Ayrton Senna Career Plot](./2024/month-01/p01-senna_carrer/plot/senna.png)

---

### **[P02 - The 80s F1 Rivalry](./2025/month-01/p02-senna_prost_mansell_piquet)**
This visualization explores the instense rivalry between four of the greatest Formula 1 drivers of the 80s:

- **Alain Prost** (4-time World Champion)
- **Ayrton Senna** (3-time World Champion)
- **Nelson Piquet** (3-time World Champion)
- **Nigel Mansell** (1-time World Champion)

Inspired by [David Sjoberg](https://github.com/davidsjoberg/ggbump) and his `ggbump` charts examples, this projects analyse the performance trends of these legendary drivers across multiple seasons.

The plot was created using `ggplot`, `ggbump` and `ggimage`.

![Project 02 Visualization](./2024/month-01/p02-senna_prost_mansell_piquet/plot/80_rivalry.png)

---

### **[P03 - Formula One World Drivers' Champions](./2025/month-01/p03-drivers_champions)**

This visualization explores all Formula 1 World Champions by season, from the first championship in 1950 to the present.

Inspired by [Georgios Karamanis](https://github.com/gkaramanis/tidytuesday/blob/master/2020/2020-week15/plots/tour-de-france.png) `tidytuesday` challenge, this visualization represents the evolution of champions throughout F1 history.
The plot was created using `ggplot`, `ggtext` and `ggimage`.

![Project 03 Visualization](./2024/month-01/p03-drivers_champions/plot/drivers_champions20250209_210401.png)

---

### **[P04 - Formula One World Drivers' Champions and Their Winning Teams](./2025/month-01/p04-drivers_champions)**

This visualization presents all Formula 1 World Champions and their winning teams from the history of the sport.

Inspired by [Georgios Karamanis](https://karaman.is/blog/2024/6/tidytuesday-202422) and his `tidytuesday` challenge, this project recreates the visualization using `ggplot2`, `tidygraph`, and `ggimage`.

The graph is structured as a dendrogram, connecting each driver to their respective winning constructor(s), with colors assigned based on the teams' historical branding.

![Project 04 Visualization](./2025/month-01/p04-constructors_champions/plot/2025_03_06_21_15_18.536183.png)

---
