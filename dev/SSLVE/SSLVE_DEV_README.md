<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>SSLVE Framework Map</title>
<style>
  @import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@300;400;500;600;700&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: 'IBM Plex Sans', sans-serif;
    background: #f5f5f0;
    color: #1a1a1a;
    min-height: 100vh;
  }

  .canvas {
    max-width: 1300px;
    margin: 0 auto;
    padding: 40px 30px 60px;
  }

  h1 {
    font-family: 'JetBrains Mono', monospace;
    font-size: 26px;
    font-weight: 600;
    color: #111;
    margin-bottom: 4px;
  }
  .subtitle {
    font-size: 13px;
    color: #888;
    letter-spacing: 0.5px;
    margin-bottom: 44px;
    font-family: 'JetBrains Mono', monospace;
  }

  .section-title {
    font-family: 'JetBrains Mono', monospace;
    font-size: 13px;
    font-weight: 600;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    color: #555;
    margin-bottom: 16px;
    padding-bottom: 6px;
    border-bottom: 2px solid #ddd;
  }

  .orch-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 16px;
    margin-bottom: 44px;
  }
  .orch-card {
    background: #fff;
    border: 2px solid #ccc;
    border-radius: 10px;
    padding: 20px;
  }
  .orch-card h3 {
    font-family: 'JetBrains Mono', monospace;
    font-size: 16px;
    font-weight: 700;
    color: #b8860b;
    margin-bottom: 10px;
  }
  .orch-card p { font-size: 13px; color: #555; line-height: 1.6; }
  .loop-seq {
    margin-top: 12px;
    font-family: 'JetBrains Mono', monospace;
    font-size: 12px;
    background: #fafaf5;
    padding: 12px 14px;
    border-radius: 6px;
    border: 1px solid #e5e5dd;
    line-height: 1.8;
  }
  .loop-seq .sp-c { color: #c0392b; font-weight: 600; }
  .loop-seq .co-c { color: #2471a3; font-weight: 600; }
  .loop-seq .bm-c { color: #7d3c98; font-weight: 600; }
  .loop-seq .lm-c { color: #1e8449; font-weight: 600; }
  .loop-seq .arr { color: #999; }
  .loop-seq .comment { color: #aaa; font-size: 11px; }

  .main-loop {
    display: grid;
    grid-template-columns: 1fr 44px 1fr 44px 1fr 44px 1fr;
    align-items: start;
    margin-bottom: 12px;
  }

  .phase-box {
    background: #fff;
    border: 2px solid #ccc;
    border-radius: 10px;
    padding: 18px 16px;
  }

  .phase-label {
    font-family: 'JetBrains Mono', monospace;
    font-size: 9px;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    color: #999;
    margin-bottom: 4px;
  }
  .phase-name {
    font-family: 'JetBrains Mono', monospace;
    font-size: 15px;
    font-weight: 700;
    margin-bottom: 10px;
  }
  .phase-desc {
    font-size: 12px;
    line-height: 1.55;
    color: #666;
    margin-bottom: 10px;
  }

  .methods {
    background: #fafaf5;
    border: 1px solid #e8e8e0;
    border-radius: 6px;
    padding: 8px 10px;
    margin-bottom: 8px;
  }
  .methods-label {
    font-family: 'JetBrains Mono', monospace;
    font-size: 9px;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: #aaa;
    margin-bottom: 4px;
  }
  .method {
    font-family: 'JetBrains Mono', monospace;
    font-size: 11px;
    color: #333;
    line-height: 1.7;
  }
  .method .ret { color: #888; }

  .contains-label {
    font-family: 'JetBrains Mono', monospace;
    font-size: 9px;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: #aaa;
    margin-top: 8px;
    margin-bottom: 3px;
  }
  .contains-item {
    font-family: 'JetBrains Mono', monospace;
    font-size: 10px;
    display: inline-block;
    padding: 2px 7px;
    border-radius: 4px;
    margin: 2px 3px 2px 0;
  }

  .io-row {
    margin-top: 10px;
    padding-top: 8px;
    border-top: 1px solid #eee;
  }
  .io-tag {
    display: inline-block;
    font-family: 'JetBrains Mono', monospace;
    font-size: 9.5px;
    padding: 2px 7px;
    border-radius: 4px;
    margin: 2px 3px 2px 0;
  }
  .io-in { background: #eef3ff; color: #3366aa; border: 1px solid #ccd8f0; }
  .io-out { background: #eefbf0; color: #2a7a3a; border: 1px solid #c0e8c8; }

  .sp .phase-name { color: #c0392b; }
  .co .phase-name { color: #2471a3; }
  .bm .phase-name { color: #7d3c98; }
  .lm .phase-name { color: #1e8449; }

  .sp { border-left: 4px solid #c0392b; }
  .co { border-left: 4px solid #2471a3; }
  .bm { border-left: 4px solid #7d3c98; }
  .lm { border-left: 4px solid #1e8449; }

  .c-sp { background: #fdf0ef; color: #c0392b; border: 1px solid #f0ccc8; }
  .c-co { background: #eef5fb; color: #2471a3; border: 1px solid #c8ddf0; }
  .c-bm { background: #f5eefb; color: #7d3c98; border: 1px solid #dcc8f0; }
  .c-lm { background: #eefbf2; color: #1e8449; border: 1px solid #c0e8c8; }
  .c-bd { background: #fdf0f8; color: #b03070; border: 1px solid #f0c8dd; }
  .c-ag { background: #fdf8ef; color: #b8860b; border: 1px solid #f0e0c0; }

  .arrow-col {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding-top: 44px;
  }
  .arrow-col svg { width: 32px; height: 32px; }
  .arrow-label {
    font-family: 'JetBrains Mono', monospace;
    font-size: 9px;
    color: #999;
    text-align: center;
    margin-top: 2px;
    line-height: 1.3;
  }

  .feedback-row {
    display: flex;
    justify-content: center;
    margin: 0 0 44px;
  }
  .feedback-arrow { width: 92%; height: 44px; position: relative; }
  .feedback-arrow svg { width: 100%; height: 100%; }
  .feedback-label {
    position: absolute;
    bottom: 2px;
    left: 50%;
    transform: translateX(-50%);
    font-family: 'JetBrains Mono', monospace;
    font-size: 10px;
    color: #1e844999;
    letter-spacing: 0.5px;
    white-space: nowrap;
  }

  .support-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 16px;
    margin-bottom: 44px;
  }
  .support-card {
    background: #fff;
    border: 2px solid #ccc;
    border-radius: 10px;
    padding: 18px 16px;
  }
  .support-card h3 {
    font-family: 'JetBrains Mono', monospace;
    font-size: 14px;
    font-weight: 700;
    margin-bottom: 6px;
  }
  .support-card p { font-size: 12px; color: #666; line-height: 1.55; margin-bottom: 8px; }
  .ag-card h3 { color: #b8860b; }
  .bd-card h3 { color: #b03070; }

  .swap-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 14px;
    margin-bottom: 44px;
  }
  .swap-card {
    background: #fff;
    border: 2px solid #ddd;
    border-radius: 10px;
    padding: 16px;
  }
  .swap-card h3 {
    font-family: 'JetBrains Mono', monospace;
    font-size: 12px;
    font-weight: 600;
    color: #333;
    margin-bottom: 8px;
  }
  .swap-card .swap-q {
    font-size: 12.5px;
    font-weight: 500;
    color: #222;
    margin-bottom: 8px;
  }
  .swap-card p {
    font-size: 11.5px;
    color: #666;
    line-height: 1.6;
  }
  .swap-card code {
    font-family: 'JetBrains Mono', monospace;
    font-size: 11px;
    background: #f0f0e8;
    padding: 1px 5px;
    border-radius: 3px;
    color: #333;
  }
  .swap-items {
    font-family: 'JetBrains Mono', monospace;
    font-size: 11px;
    color: #444;
    margin-top: 6px;
    line-height: 1.8;
  }
  .swap-num {
    display: inline-block;
    width: 18px;
    height: 18px;
    line-height: 18px;
    text-align: center;
    border-radius: 50%;
    font-size: 10px;
    font-weight: 700;
    margin-right: 4px;
  }

  .legend {
    display: flex;
    gap: 20px;
    flex-wrap: wrap;
    padding-top: 14px;
    border-top: 2px solid #ddd;
  }
  .legend-item {
    display: flex;
    align-items: center;
    gap: 6px;
    font-family: 'JetBrains Mono', monospace;
    font-size: 10px;
    color: #777;
  }
  .legend-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
  }
</style>
</head>
<body>
<div class="canvas">

  <h1>SSLVE Framework</h1>
  <div class="subtitle">Self-Supervised Latent Variable Evolution — Architecture Map</div>

  <!-- ORCHESTRATORS -->
  <div class="section-title">Orchestrators</div>
  <div class="orch-grid">
    <div class="orch-card">
      <h3>SSLVE</h3>
      <p>Full loop with latent module training each step.</p>
      <div class="loop-seq">
        <span class="comment"># each step:</span><br>
        thetas = <span class="sp-c">SP</span>.sample(latent_module, collector, behavior_matching)<br>
        <span class="arr">for</span> θ <span class="arr">in</span> thetas:<br>
        &nbsp;&nbsp;agent = <span class="sp-c">SP</span>.make_agent(θ)<br>
        &nbsp;&nbsp;info = <span class="co-c">CO</span>.collect(agent)<br>
        <span class="bm-c">BM</span>.update(thetas, infos)<br>
        <span class="lm-c">LM</span>.fit(BM.dataset, BM.bin_ids, BM.bins_idx)
      </div>
      <div class="contains-label">Contains</div>
      <span class="contains-item c-sp">SP</span>
      <span class="contains-item c-co">CO</span>
      <span class="contains-item c-bm">BM</span>
      <span class="contains-item c-lm">LM</span>
    </div>
    <div class="orch-card">
      <h3>MAPElite</h3>
      <p>MAP-Elites without latent module.</p>
      <div class="loop-seq">
        <span class="comment"># each step:</span><br>
        thetas = <span class="sp-c">SP</span>.sample(collector, behavior_matching)<br>
        <span class="arr">for</span> θ <span class="arr">in</span> thetas:<br>
        &nbsp;&nbsp;agent = <span class="sp-c">SP</span>.make_agent(θ)<br>
        &nbsp;&nbsp;info = <span class="co-c">CO</span>.collect(agent)<br>
        <span class="bm-c">BM</span>.update(thetas, infos)
      </div>
      <div class="contains-label">Contains</div>
      <span class="contains-item c-sp">SP</span>
      <span class="contains-item c-co">CO</span>
      <span class="contains-item c-bm">BM</span>
    </div>
  </div>

  <!-- MAIN LOOP -->
  <div class="section-title">Core Components · Data Flow</div>
  <div class="main-loop">

    <div class="phase-box sp">
      <div class="phase-label">Step 1 · Search</div>
      <div class="phase-name">SearchPhase (SP)</div>
      <div class="phase-desc">Generates candidate θ vectors. Selects parents from archive, applies variation in parameter or latent space.</div>
      <div class="methods">
        <div class="methods-label">Core Methods</div>
        <div class="method">sample(**kwargs) <span class="ret">→ List[np.array]</span></div>
        <div class="method">make_agent(θ) <span class="ret">→ Agent</span></div>
      </div>
      <div class="contains-label">Uses internally</div>
      <span class="contains-item c-ag">Agent class</span>
      <div class="io-row">
        <span class="io-tag io-in">BM.bins_idx</span>
        <span class="io-tag io-in">BM.dataset</span>
        <span class="io-tag io-in">LM.encode</span>
        <span class="io-tag io-in">LM.decode</span>
        <span class="io-tag io-out">List[θ]</span>
      </div>
    </div>

    <div class="arrow-col">
      <svg viewBox="0 0 32 32"><path d="M4 16 L24 16 M19 11 L24 16 L19 21" stroke="#999" fill="none" stroke-width="1.5"/></svg>
      <div class="arrow-label">θ →<br>agent</div>
    </div>

    <div class="phase-box co">
      <div class="phase-label">Step 2 · Evaluate</div>
      <div class="phase-name">Collector (CO)</div>
      <div class="phase-desc">Runs agent in environment. Returns raw per-episode measurements as info dict.</div>
      <div class="methods">
        <div class="methods-label">Core Methods</div>
        <div class="method">collect(agent) <span class="ret">→ dict</span></div>
      </div>
      <div class="io-row">
        <span class="io-tag io-in">agent</span>
        <span class="io-tag io-out">info dict</span>
      </div>
    </div>

    <div class="arrow-col">
      <svg viewBox="0 0 32 32"><path d="M4 16 L24 16 M19 11 L24 16 L19 21" stroke="#999" fill="none" stroke-width="1.5"/></svg>
      <div class="arrow-label">θ, info</div>
    </div>

    <div class="phase-box bm">
      <div class="phase-label">Step 3 · Archive</div>
      <div class="phase-name">BehaviorMatching (BM)</div>
      <div class="phase-desc">Computes BD via contained BehaviorDescriptor, evaluates fitness, maintains top-k archive per bin.</div>
      <div class="methods">
        <div class="methods-label">Core Methods</div>
        <div class="method">update(thetas, infos)</div>
        <div class="method">coverage() <span class="ret">→ float</span></div>
        <div class="method">fitness_stats() <span class="ret">→ (min,mean,max)</span></div>
      </div>
      <div class="methods" style="margin-top:6px;">
        <div class="methods-label">State (exposed to SP & LM)</div>
        <div class="method">dataset <span class="ret">: List[np.array]</span></div>
        <div class="method">bin_ids <span class="ret">: List[bin_id]</span></div>
        <div class="method">bins_idx <span class="ret">: dict{bin_id → [indices]}</span></div>
        <div class="method">fitnesses <span class="ret">: List[float]</span></div>
      </div>
      <div class="contains-label">Contains</div>
      <span class="contains-item c-bd">BehaviorDescriptor</span>
      <div class="io-row">
        <span class="io-tag io-in">θ, info</span>
        <span class="io-tag io-in">fitness_fn</span>
        <span class="io-tag io-out">dataset</span>
        <span class="io-tag io-out">bins_idx</span>
      </div>
    </div>

    <div class="arrow-col">
      <svg viewBox="0 0 32 32"><path d="M4 16 L24 16 M19 11 L24 16 L19 21" stroke="#999" fill="none" stroke-width="1.5"/></svg>
      <div class="arrow-label">dataset<br>bins</div>
    </div>

    <div class="phase-box lm">
      <div class="phase-label">Step 4 · Learn</div>
      <div class="phase-name">LatentModule (LM)</div>
      <div class="phase-desc">Trains representation on archive θ vectors. Provides encode/decode used by SP for latent-space variation.</div>
      <div class="methods">
        <div class="methods-label">Core Methods</div>
        <div class="method">fit(dataset, bin_ids, bins) <span class="ret">→ history</span></div>
        <div class="method">encode(x) <span class="ret">→ z</span></div>
        <div class="method">encode_dist(x) <span class="ret">→ (μ, logvar)</span></div>
        <div class="method">decode(z) <span class="ret">→ x̂</span></div>
      </div>
      <div class="io-row">
        <span class="io-tag io-in">dataset</span>
        <span class="io-tag io-in">bin_ids</span>
        <span class="io-tag io-out">encoder</span>
        <span class="io-tag io-out">decoder</span>
      </div>
    </div>
  </div>

  <div class="feedback-row">
    <div class="feedback-arrow">
      <svg viewBox="0 0 1000 44">
        <defs>
          <marker id="ah" markerWidth="8" markerHeight="6" refX="8" refY="3" orient="auto">
            <polygon points="0 0, 8 3, 0 6" fill="#1e844966"/>
          </marker>
        </defs>
        <path d="M920 4 C920 32, 880 40, 80 40 C40 40, 20 32, 20 12"
              stroke="#1e844944" fill="none" stroke-width="1.5"
              stroke-dasharray="6 4" marker-end="url(#ah)"/>
      </svg>
      <div class="feedback-label">LM encoder/decoder feeds back to SP for latent-space variation</div>
    </div>
  </div>

  <!-- SUPPORTING -->
  <div class="section-title">Supporting Abstractions (used inside above components)</div>
  <div class="support-grid">
    <div class="support-card ag-card">
      <h3>Agent</h3>
      <p>Converts flat θ → executable policy. Created by SP.make_agent(), consumed by CO.collect().</p>
      <div class="methods">
        <div class="methods-label">Core Methods</div>
        <div class="method">set_weights(flat_weights)</div>
        <div class="method">act(obs) <span class="ret">→ action</span></div>
        <div class="method">get_weight_dim() <span class="ret">→ int</span></div>
      </div>
    </div>
    <div class="support-card bd-card">
      <h3>BehaviorDescriptor (BD)</h3>
      <p>Contained inside BM. Extracts continuous BD from info dict, then discretizes to bin ID.</p>
      <div class="methods">
        <div class="methods-label">Core Methods</div>
        <div class="method">describe(info) <span class="ret">→ descriptor</span></div>
        <div class="method">discretize(descriptor) <span class="ret">→ bin_id</span></div>
        <div class="method">total_bins() <span class="ret">→ int</span></div>
      </div>
    </div>
  </div>

  <!-- SWAP GUIDE -->
  <div class="section-title">Swap Guide — What to Implement for Each Extension</div>
  <div class="swap-grid">
    <div class="swap-card">
      <div class="swap-q">① New task environment</div>
      <div class="swap-items">
        <span class="swap-num" style="background:#2471a322;color:#2471a3;">1</span> New <code>Collector</code> — implement <code>collect(agent) → info</code><br>
        <span class="swap-num" style="background:#b0307022;color:#b03070;">2</span> New <code>BehaviorDescriptor</code> — implement <code>describe(info)</code>, <code>discretize()</code><br>
        <span class="swap-num" style="background:#b8860b22;color:#b8860b;">3</span> New <code>Agent</code> if needed (e.g. different obs/act interface)<br>
      </div>
      <p style="margin-top:8px;">SP, BM, LM remain unchanged.</p>
    </div>

    <div class="swap-card">
      <div class="swap-q">② New search / evolution method</div>
      <div class="swap-items">
        <span class="swap-num" style="background:#c0392b22;color:#c0392b;">1</span> New <code>SearchPhase</code> — implement <code>sample(**kwargs)</code> and <code>make_agent(θ)</code><br>
      </div>
      <p style="margin-top:8px;">Must accept <code>latent_module</code>, <code>collector</code>, <code>behavior_matching</code> as kwargs (use or ignore). CO, BM, LM, Agent unchanged.</p>
    </div>

    <div class="swap-card">
      <div class="swap-q">③ Different behavior definition (same task)</div>
      <div class="swap-items">
        <span class="swap-num" style="background:#b0307022;color:#b03070;">1</span> New <code>BehaviorDescriptor</code> — implement <code>describe(info)</code>, <code>discretize()</code>, <code>total_bins()</code><br>
      </div>
      <p style="margin-top:8px;">Same Collector (same info dict), just different BD extraction/discretization. Pass to BM constructor.</p>
    </div>

    <div class="swap-card">
      <div class="swap-q">④ Different behavior matching / binning</div>
      <div class="swap-items">
        <span class="swap-num" style="background:#7d3c9822;color:#7d3c98;">1</span> New <code>BehaviorMatching</code> — implement <code>update(thetas, infos)</code><br>
      </div>
      <p style="margin-top:8px;">Must expose <code>dataset</code>, <code>bin_ids</code>, <code>bins_idx</code>, <code>fitnesses</code>, <code>bins</code> for SP and LM to read. Contains a BD instance.</p>
    </div>
  </div>

  <div class="legend">
    <div class="legend-item"><div class="legend-dot" style="background:#c0392b;"></div>SearchPhase (SP)</div>
    <div class="legend-item"><div class="legend-dot" style="background:#2471a3;"></div>Collector (CO)</div>
    <div class="legend-item"><div class="legend-dot" style="background:#7d3c98;"></div>BehaviorMatching (BM)</div>
    <div class="legend-item"><div class="legend-dot" style="background:#1e8449;"></div>LatentModule (LM)</div>
    <div class="legend-item"><div class="legend-dot" style="background:#b8860b;"></div>Agent</div>
    <div class="legend-item"><div class="legend-dot" style="background:#b03070;"></div>BehaviorDescriptor (BD)</div>
  </div>

</div>
</body>
</html>
