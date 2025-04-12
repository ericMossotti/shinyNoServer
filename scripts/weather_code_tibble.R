
# Weather code definition tibble ----

weather_code_tibble <- function(){
     code_frame <- tibble::tibble(
          weather_code = c(
               '0',
               '1',
               '2',
               '3',
               '45',
               '48',
               '51',
               '53',
               '55',
               '56',
               '57',
               '61',
               '63',
               '65',
               '66',
               '67',
               '71',
               '73',
               '75',
               '77',
               '80',
               '81',
               '82',
               '85',
               '86',
               '95',
               '96',
               '99'
          ),
          
          description = c(
               'Clear sky',
               'Mainly clear',
               'Partly cloudy',
               'Overcast',
               'Fog',
               'Depositing rime fog',
               'Drizzle: light',
               'Drizzle: moderate',
               'Drizzle: dense',
               'Freezing drizzle: light',
               'Freezing drizzle: dense',
               'Rain: slight',
               'Rain: moderate',
               'Rain: heavy',
               'Freezing rain: light',
               'Freezing rain: heavy',
               'Snow fall: slight',
               'Snow fall: moderate',
               'Snow fall: heavy',
               'Snow grains',
               'Rain showers: slight',
               'Rain showers: moderate',
               'Rain showers: violent',
               'Snow showers: slight',
               'Snow showers: heavy',
               'Thunderstorm: slight or moderate',
               'Thunderstorm with slight hail',
               'Thunderstorm with heavy hail'
          ),
          
          implication = c(
               "Normal operations - No restrictions",              # Clear sky
               "Normal operations - Increased vigilance",          # Mainly clear
               "Normal operations - Monitor weather updates",      # Partly cloudy
               "Reduced visibility - Maintain safe following distance", # Overcast
               "Speed reduction required - Fog lights mandatory",  # Fog
               "Speed reduction required - Extreme caution",        # Depositing rime fog
               "Potential minor delays - Road surface slickness",   # Drizzle: light
               "Speed restrictions - 15% reduction recommended",    # Drizzle: moderate
               "Mandatory speed reduction - 25%+",                 # Drizzle: dense
               "Chain requirement - Level 1 traction advisory",     # Freezing drizzle: light
               "Road closure likely - Avoid non-essential travel",  # Freezing drizzle: dense
               "Increased stopping distance - 10% speed reduction", # Rain: slight
               "15-20% speed reduction - Check tire tread",         # Rain: moderate
               "25%+ speed reduction - Possible detour routing",    # Rain: heavy
               "Mandatory chains - Temperature monitoring",         # Freezing rain: light
               "Road closure imminent - Immediate stop advised",    # Freezing rain: heavy
               "15% speed reduction - Traction control engaged",    # Snow fall: slight
               "25% speed reduction - Chain requirement possible",  # Snow fall: moderate
               "Road closure likely - Abandon shipment staging",    # Snow fall: heavy
               "Speed restriction - Watch for black ice",           # Snow grains
               "Increased following distance - 4-second rule",      # Rain showers: slight
               "20% speed reduction - Avoid lane changes",          # Rain showers: moderate
               "Immediate parking advised - Flash flood risk",      # Rain showers: violent
               "Chain requirement - Trailer brake check",           # Snow showers: slight
               "Road closure protocol activated",                   # Snow showers: heavy
               "Delay shipments - No open-top trailers",            # Thunderstorm: slight/mod
               "Immediate stop - Seek shelter",                     # Thunderstorm w/ slight hail
               "Catastrophic risk - Emergency protocols"            # Thunderstorm w/ heavy hail
          ),
          
          risk_score = c(
               0.1,  # Clear sky
               0.15, # Mainly clear
               0.2,  # Partly cloudy
               0.25, # Overcast
               0.4,  # Fog
               0.5,  # Depositing rime fog
               0.3,  # Drizzle: light
               0.35, # Drizzle: moderate
               0.45, # Drizzle: dense
               0.55, # Freezing drizzle: light
               0.8,  # Freezing drizzle: dense
               0.3,  # Rain: slight
               0.4,  # Rain: moderate
               0.6,  # Rain: heavy
               0.65, # Freezing rain: light
               0.85, # Freezing rain: heavy
               0.4,  # Snow fall: slight
               0.6,  # Snow fall: moderate
               0.75, # Snow fall: heavy
               0.5,  # Snow grains
               0.35, # Rain showers: slight
               0.5,  # Rain showers: moderate
               0.7,  # Rain showers: violent
               0.6,  # Snow showers: slight
               0.8,  # Snow showers: heavy
               0.65, # Thunderstorm: slight/mod
               0.85, # Thunderstorm w/ slight hail
               0.95  # Thunderstorm w/ heavy hail
          ),
          
          dot_compliance = c(
               "§392.14(a)",              # Clear sky
               "§392.14(a)",              # Mainly clear
               "§392.14(a)",              # Partly cloudy
               "§392.14(b)",              # Overcast
               "§392.14(b)+§393.75(c)",   # Fog
               "§392.14(c)",              # Depositing rime fog
               "§392.71(a)",              # Drizzle: light
               "§392.71(b)",              # Drizzle: moderate
               "§392.71(c)",              # Drizzle: dense
               "§392.16(a)",              # Freezing drizzle: light
               "§392.16(c)",              # Freezing drizzle: dense
               "§392.71(a)",              # Rain: slight
               "§392.71(b)",              # Rain: moderate
               "§392.71(c)",              # Rain: heavy
               "§392.16(b)+§393.95(d)",   # Freezing rain: light
               "§392.16(c)",              # Freezing rain: heavy
               "§392.14(b)+§393.95(a)",   # Snow fall: slight
               "§392.14(c)+§393.95(b)",   # Snow fall: moderate
               "§392.16(c)",              # Snow fall: heavy
               "§392.14(c)",              # Snow grains
               "§392.14(b)",              # Rain showers: slight
               "§392.14(c)",              # Rain showers: moderate
               "§392.16(c)",              # Rain showers: violent
               "§393.95(c)",              # Snow showers: slight
               "§392.16(c)",              # Snow showers: heavy
               "§392.14(d)+§393.75(e)",   # Thunderstorm: slight/mod
               "§392.16(c)",              # Thunderstorm w/ slight hail
               "§392.16(e)"               # Thunderstorm w/ heavy hail
          ),
          
          severity = cut(
               risk_score,
               breaks = c(0, 0.3, 0.5, 0.7, 1),
               labels = c("Low", "Moderate", "High", "Critical")
          ),
          
          insurance_surcharge = c(
               0,    # Clear sky
               0,    # Mainly clear
               0.05, # Partly cloudy (5%)
               0.07, # Overcast (7%)
               0.1,  # Fog (10%)
               0.15, # Rime fog (15%)
               0.08, # Light drizzle (8%)
               0.12, # Moderate drizzle (12%)
               0.18, # Dense drizzle (18%)
               0.25, # Freezing drizzle light (25%)
               0.4,  # Freezing drizzle dense (40%)
               0.1,  # Rain slight (10%)
               0.15, # Rain moderate (15%)
               0.25, # Rain heavy (25%)
               0.35, # Freezing rain light (35%)
               0.5,  # Freezing rain heavy (50%)
               0.2,  # Snow slight (20%)
               0.3,  # Snow moderate (30%)
               0.45, # Snow heavy (45%)
               0.25, # Snow grains (25%)
               0.12, # Rain showers slight (12%)
               0.2,  # Rain showers moderate (20%)
               0.35, # Rain showers violent (35%)
               0.3,  # Snow showers slight (30%)
               0.5,  # Snow showers heavy (50%)
               0.4,  # Thunderstorm (40%)
               0.6,  # Thunderstorm w/ slight hail (60%)
               0.8   # Thunderstorm w/ heavy hail (80%)
          ),
          
          fuel_multiplier = c(
               1.0,  # Clear sky
               1.0,  # Mainly clear
               1.03, # Partly cloudy (3%)
               1.05, # Overcast (5%)
               1.12, # Fog (12%)
               1.15, # Rime fog (15%)
               1.07, # Light drizzle (7%)
               1.1,  # Moderate drizzle (10%)
               1.15, # Dense drizzle (15%)
               1.25, # Freezing drizzle light (25%)
               1.4,  # Freezing drizzle dense (40%)
               1.08, # Rain slight (8%)
               1.12, # Rain moderate (12%)
               1.2,  # Rain heavy (20%)
               1.3,  # Freezing rain light (30%)
               1.5,  # Freezing rain heavy (50%)
               1.15, # Snow slight (15%)
               1.25, # Snow moderate (25%)
               1.4,  # Snow heavy (40%)
               1.2,  # Snow grains (20%)
               1.1,  # Rain showers slight (10%)
               1.15, # Rain showers moderate (15%)
               1.3,  # Rain showers violent (30%)
               1.25, # Snow showers slight (25%)
               1.45, # Snow showers heavy (45%)
               1.35, # Thunderstorm (35%)
               1.6,  # Thunderstorm w/ slight hail (60%)
               2.0   # Thunderstorm w/ heavy hail (100%)
          ),
          
          route_delay_factor = c(
               1.0,  # Clear sky
               1.0,  # Mainly clear
               1.00,  # Partly cloudy
               1.01,  # Overcast
               1.05,  # Fog
               1.08,  # Rime fog
               1.03,  # Light drizzle
               1.06,  # Moderate drizzle
               1.2,  # Dense drizzle
               1.25,  # Freezing drizzle light
               1.4,  # Freezing drizzle dense
               1.04,  # Rain slight
               1.08,  # Rain moderate
               1.25,  # Rain heavy
               1.3,  # Freezing rain light
               1.5,  # Freezing rain heavy
               1.2,  # Snow slight
               1.3,  # Snow moderate
               1.45,  # Snow heavy
               1.25,  # Snow grains
               1.05,  # Rain showers slight
               1.2,  # Rain showers moderate
               1.35,  # Rain showers violent
               1.3,  # Snow showers slight
               1.5,  # Snow showers heavy
               1.3,  # Thunderstorm
               1.6,  # Thunderstorm w/ slight hail
               2.0  # Thunderstorm w/ heavy hail
          ),
          
          # New Labor & Equipment Columns
          safety_inspections = c(
               "Pre-trip only",                    # Clear sky
               "Pre-trip + mid-trip visual",       # Mainly clear
               "Pre-trip + brake check",           # Partly cloudy
               "Pre-trip + hourly tire checks",    # Overcast
               "Pre-trip + fog light checks",      # Fog
               "Pre-trip + 30-min interval checks",# Rime fog
               "Pre-trip + 2hr brake tests",       # Drizzle: light
               "Pre-trip + 1hr brake tests",       # Drizzle: moderate
               "Pre-trip + 30min brake tests",     # Drizzle: dense
               "Pre-trip + axle temp monitoring",  # Freezing drizzle: light
               "Continuous monitoring required",   # Freezing drizzle: dense
               "Pre-trip + wiper checks",          # Rain: slight
               "Pre-trip + 2hr wiper checks",      # Rain: moderate
               "Pre-trip + 30min wiper checks",    # Rain: heavy
               "Pre-trip + chain integrity checks",# Freezing rain: light
               "Roadside inspections mandatory",   # Freezing rain: heavy
               "Pre-trip + tire chain prep",       # Snow fall: slight
               "Pre-trip + hourly chain checks",   # Snow fall: moderate
               "Continuous chain monitoring",      # Snow fall: heavy
               "Pre-trip + sanding required",      # Snow grains
               "Pre-trip + drainage checks",       # Rain showers: slight
               "Pre-trip + undercarriage checks",  # Rain showers: moderate
               "Abort trip + full inspection",     # Rain showers: violent
               "Pre-trip + plow attachment",       # Snow showers: slight
               "Roadside de-icing required",       # Snow showers: heavy
               "Pre-trip + lightning protocol",    # Thunderstorm: slight/mod
               "Immediate shelter + inspection",   # Thunderstorm w/ slight hail
               "Post-storm forensic inspection"    # Thunderstorm w/ heavy hail
          ),
          
          driver_wage_premium = c(
               0.00,  # Clear sky
               0.00,   # Mainly clear
               0.05,   # Partly cloudy (+5%)
               0.07,   # Overcast (+7%)
               0.15,   # Fog (+15%)
               0.20,   # Rime fog (+20%)
               0.10,   # Drizzle: light (+10%)
               0.12,   # Drizzle: moderate (+12%)
               0.18,   # Drizzle: dense (+18%)
               0.25,   # Freezing drizzle: light (+25%)
               0.40,   # Freezing drizzle: dense (+40%)
               0.10,   # Rain: slight (+10%)
               0.15,   # Rain: moderate (+15%)
               0.25,   # Rain: heavy (+25%)
               0.35,   # Freezing rain: light (+35%)
               0.50,   # Freezing rain: heavy (+50%)
               0.20,   # Snow fall: slight (+20%)
               0.30,   # Snow fall: moderate (+30%)
               0.45,   # Snow fall: heavy (+45%)
               0.25,   # Snow grains (+25%)
               0.12,   # Rain showers: slight (+12%)
               0.20,   # Rain showers: moderate (+20%)
               0.35,   # Rain showers: violent (+35%)
               0.30,   # Snow showers: slight (+30%)
               0.50,   # Snow showers: heavy (+50%)
               0.40,   # Thunderstorm (+40%)
               0.60,   # Thunderstorm w/ slight hail (+60%)
               0.80    # Thunderstorm w/ heavy hail (+80%)
          ),
          
          equipment_wear_factor = c(
               1.0,   # Clear sky
               1.02,  # Mainly clear (+2%)
               1.05,  # Partly cloudy (+5%)
               1.07,  # Overcast (+7%)
               1.15,  # Fog (+15%)
               1.20,  # Rime fog (+20%)
               1.10,  # Drizzle: light (+10%)
               1.12,  # Drizzle: moderate (+12%)
               1.18,  # Drizzle: dense (+18%)
               1.25,  # Freezing drizzle: light (+25%)
               1.40,  # Freezing drizzle: dense (+40%)
               1.12,  # Rain: slight (+12%)
               1.15,  # Rain: moderate (+15%)
               1.25,  # Rain: heavy (+25%)
               1.35,  # Freezing rain: light (+35%)
               1.50,  # Freezing rain: heavy (+50%)
               1.20,  # Snow fall: slight (+20%)
               1.30,  # Snow fall: moderate (+30%)
               1.45,  # Snow fall: heavy (+45%)
               1.25,  # Snow grains (+25%)
               1.10,  # Rain showers: slight (+10%)
               1.15,  # Rain showers: moderate (+15%)
               1.30,  # Rain showers: violent (+30%)
               1.25,  # Snow showers: slight (+25%)
               1.45,  # Snow showers: heavy (+45%)
               1.35,  # Thunderstorm (+35%)
               1.60,  # Thunderstorm w/ slight hail (+60%)
               2.0    # Thunderstorm w/ heavy hail (+100%)
          ),
          
          carbon_multiplier = c(
               1.00,  # Clear sky
               1.01,  # Mainly clear (+1%)
               1.03,  # Partly cloudy (+3%)
               1.05,  # Overcast (+5%)
               1.12,  # Fog (+12%)
               1.15,  # Rime fog (+15%)
               1.07,  # Drizzle: light (+7%)
               1.10,  # Drizzle: moderate (+10%)
               1.15,  # Drizzle: dense (+15%)
               1.22,  # Freezing drizzle: light (+22%)
               1.35,  # Freezing drizzle: dense (+35%)
               1.08,  # Rain: slight (+8%)
               1.12,  # Rain: moderate (+12%)
               1.20,  # Rain: heavy (+20%)
               1.28,  # Freezing rain: light (+28%)
               1.45,  # Freezing rain: heavy (+45%)
               1.15,  # Snow fall: slight (+15%)
               1.25,  # Snow fall: moderate (+25%)
               1.40,  # Snow fall: heavy (+40%)
               1.20,  # Snow grains (+20%)
               1.10,  # Rain showers: slight (+10%)
               1.15,  # Rain showers: moderate (+15%)
               1.30,  # Rain showers: violent (+30%)
               1.25,  # Snow showers: slight (+25%)
               1.40,  # Snow showers: heavy (+40%)
               1.35,  # Thunderstorm (+35%)
               1.55,  # Thunderstorm w/ slight hail (+55%)
               1.80   # Thunderstorm w/ heavy hail (+80%)
          ),
          
          # Bridge Weight Restrictions (FHWA Load Rating Manual)
          bridge_weight_limit = c(
               1.00, 1.00, 0.98, 0.95, 0.90, 0.85, 0.92, 0.88, 0.82, 0.75, 0.60,
               0.93, 0.87, 0.78, 0.65, 0.50, 0.85, 0.72, 0.55, 0.80, 0.91, 0.86,
               0.60, 0.70, 0.45, 0.68, 0.40, 0.30
          ),
          
          # Toll Multipliers (IBTTA 2023 Storm Surcharge Index)
          toll_multiplier = c(
               1.00, 1.00, 1.05, 1.07, 1.15, 1.25, 1.10, 1.15, 1.22, 1.35, 2.00,
               1.12, 1.18, 1.30, 1.45, 1.80, 1.20, 1.35, 1.60, 1.25, 1.13, 1.20,
               1.70, 1.40, 2.10, 1.55, 2.30, 3.00
          ),
          
          # Border Crossing Delays (CBP TRIP Data)
          border_delay_hours = c(
               0.0, 0.0, 0.5, 0.7, 1.2, 2.0, 0.8, 1.1, 1.8, 2.5, 6.0,
               0.9, 1.3, 2.2, 3.5, 8.0, 1.5, 2.8, 5.0, 1.7, 1.0, 1.5,
               4.0, 2.5, 7.0, 3.0, 9.0, 12.0
          ),
          
          # API Endpoints
          reroute_api = c(
               NA_character_,  # Clear sky
               NA_character_,  # Mainly clear
               "HERE Weather API v3",  # Partly cloudy
               "HERE Weather API v3",  # Overcast
               "FHWA ARCHIS Live",  # Fog
               "FHWA ARCHIS Live",  # Rime fog
               "Google Maps Directions",  # Drizzle
               "Google Maps Directions",  # Drizzle
               "Google Maps Directions",  # Drizzle
               "FMCSA SMS API",  # Freezing drizzle
               "FMCSA SMS API",  # Freezing drizzle
               "USDOT NTAD",  # Rain
               "USDOT NTAD",  # Rain
               "USDOT NTAD",  # Rain
               "FMCSA SMS API",  # Freezing rain
               "FMCSA SMS API",  # Freezing rain
               "FHWA RWIS",  # Snow
               "FHWA RWIS",  # Snow
               "FHWA RWIS",  # Snow
               "USGS Streamflow",  # Snow grains
               "NOAA NOWData",  # Rain showers
               "NOAA NOWData",  # Rain showers
               "USGS Flood Events",  # Rain showers violent
               "FHWA CCAP",  # Snow showers
               "FHWA CCAP",  # Snow showers
               "NWS CAP Alerts",  # Thunderstorm
               "NWS CAP Alerts",  # Thunderstorm hail
               "DHS HSIN"  # Severe hail
               )
          )
          
          return(code_frame)
     }
