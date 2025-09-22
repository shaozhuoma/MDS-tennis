###  ATP Singles-match Variable Dictionary

*(🔸 fields marked with “!” are sometimes cast to a different statistical type in practice—e.g., one-hot encoded as nominal instead of ordinal, or parsed as dates instead of treated as ordered integers.)*

------

#### **Ⅰ Event / match context**

| Field           | Type       | Notes                                    |
| --------------- | ---------- | ---------------------------------------- |
| `tourney_id`    | Nominal    | unique event ID                          |
| `tourney_name`  | Nominal    | tournament name                          |
| `surface`       | Nominal    | court surface                            |
| `draw_size`     | Discrete 🔸 | 16 / 32 / 64 / 128, often used ordinally |
| `tourney_level` | Nominal    | G, M, A, C, S …                          |
| `tourney_date`  | Ordinal 🔸  | YYYYMMDD (often parsed to `Date`)        |
| `match_num`     | Discrete   | match index within draw                  |

------

#### **Ⅱ Winner information**

| Field          | Type       | Notes                     |
| -------------- | ---------- | ------------------------- |
| `winner_id`    | Nominal    |                           |
| `winner_seed`  | Ordinal 🔸  | seed number (can one-hot) |
| `winner_entry` | Nominal    | WC, Q, LL, PR …           |
| `winner_name`  | Nominal    |                           |
| `winner_hand`  | Nominal    | R / L / U                 |
| `winner_ht`    | Continuous | cm                        |
| `winner_ioc`   | Nominal    | country code              |
| `winner_age`   | Continuous | years                     |

------

#### **Ⅲ Loser information**

| Field         | Type       | Notes |
| ------------- | ---------- | ----- |
| `loser_id`    | Nominal    |       |
| `loser_seed`  | Ordinal 🔸  |       |
| `loser_entry` | Nominal    |       |
| `loser_name`  | Nominal    |       |
| `loser_hand`  | Nominal    |       |
| `loser_ht`    | Continuous |       |
| `loser_ioc`   | Nominal    |       |
| `loser_age`   | Continuous |       |

------

#### **Ⅳ Match outcome / basics**

| Field     | Type         | Notes                                   |
| --------- | ------------ | --------------------------------------- |
| `score`   | Nominal      |                                         |
| `best_of` | Discrete     |                                         |
| `round`   | Ordinal      |                                         |
| `minutes` | Continuous 🔸 | integer minutes but treated as duration |

------

#### **Ⅴ Winner match stats** 

*(all team counts → Discrete)*

```
w_ace`, `w_df`, `w_svpt`, `w_1stIn`, `w_1stWon`, `w_2ndWon`,
 `w_SvGms`, `w_bpSaved`, `w_bpFaced
```

------

#### **Ⅵ Loser match stats**

*(all team counts → Discrete)*

```
l_ace`, `l_df`, `l_svpt`, `l_1stIn`, `l_1stWon`, `l_2ndWon`,
 `l_SvGms`, `l_bpSaved`, `l_bpFaced
```

------

#### **Ⅶ Ranking & points**

| Field                | Type       | Notes                       |
| -------------------- | ---------- | --------------------------- |
| `winner_rank`        | Ordinal 🔸  | often modeled as continuous |
| `winner_rank_points` | Continuous |                             |
| `loser_rank`         | Ordinal 🔸  |                             |
| `loser_rank_points`  | Continuous |                             |

------

### **Category counts**

| Category   | Variables |
| ---------- | --------- |
| Nominal    | **16**    |
| Ordinal    | **5**     |
| Discrete   | **20**    |
| Continuous | **8**     |
| **Total**  | **49** ✔️  |

> **Legend for 🔸**
>  • `draw_size`—technically counts but usually interpreted ordinally.
>  • `tourney_date`—ordered integer, usually converted to `Date`.
>  • `winner_seed` / `loser_seed`—ordinal by default, sometimes one-hot.
>  • `minutes`—stored as integer minutes yet treated as continuous duration.
>  • `winner_rank` / `loser_rank`—rank order, often fed to models as continuous magnitude