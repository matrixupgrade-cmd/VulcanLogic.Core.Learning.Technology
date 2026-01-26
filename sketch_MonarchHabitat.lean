/-!
===============================================================================
Monarch Basin + Nested Attractor Simulation with Visual Dashboard
Author: Sean Timothy (cleaned & improved edition)
Purpose:
  Basin population dynamics + gardener fallback + attractor cycles
  Deterministic, adaptive agent, and stochastic-ready paths
  Simple visual ASCII/emoji-style dashboard
===============================================================================
-/

import Mathlib.Data.Real.Basic
import Mathlib.Data.List.Basic

-- =========================================
-- Basin / Population
-- =========================================

structure Basin where
  eggs           : Nat
  caterpillars   : Nat
  adults         : Nat
  viability      : Float
  predator_level : Float
  option_history : List (Nat × Nat × Nat)
  deriving Repr, BEq

def total_pop (b : Basin) : Nat := b.eggs + b.caterpillars + b.adults

-- =========================================
-- Gardener (history-aware fallback + viability tweak)
-- =========================================

def gardener_intervention (b : Basin) : Basin :=
  let fallback :=
    match b.option_history with
    | [] => (b.eggs, b.caterpillars, b.adults)
    | h::_ => h
  let density_pressure := if total_pop b > 800 then 0.04 else 0.0
  let adjusted_viability := (b.viability + 0.05 - 0.02 * b.predator_level - density_pressure).clamp 0.08 1.0
  { b with
    eggs := fallback.1
    caterpillars := fallback.2
    adults := fallback.3
    viability := adjusted_viability
    option_history := ((fallback.1, fallback.2, fallback.3) :: b.option_history).take 6 }

-- =========================================
-- Basin life-cycle (core transition)
-- =========================================

def update_basin (b : Basin) (eggs_surv r_cat : Float) : Basin :=
  let survived_eggs     := (b.eggs.toFloat     * eggs_surv).floor.toNat
  let new_caterpillars  := (survived_eggs.toFloat * r_cat).floor.toNat

  let adult_surv       := (0.75 - 0.1 * b.predator_level).max 0.05
  let survived_adults  := (b.adults.toFloat * adult_surv).floor.toNat

  let eggs_per_adult   := 28.0 * b.viability
  let raw_new_eggs     := (survived_adults.toFloat * eggs_per_adult).floor.toNat
  let new_eggs         := min raw_new_eggs 1800   -- soft cap to avoid instant explosion

  let new_history := (new_eggs, new_caterpillars, survived_adults) :: b.option_history

  { b with
    eggs         := new_eggs
    caterpillars := new_caterpillars
    adults       := survived_adults
    option_history := new_history.take 6 }

-- =========================================
-- Nested Attractor mechanics
-- =========================================

structure SystemState where
  basin        : Basin
  invariants   : Float
  optionality  : Float
  deriving Repr, BEq

def total_budget (s : SystemState) : Float := s.invariants + s.optionality

def grind   (s : SystemState) (factor : Float := 0.9) : SystemState := { s with invariants := factor * s.invariants }
def repair  (s : SystemState) : SystemState := { s with invariants := min 1.0 (s.invariants + 0.05 * (1.0 - s.invariants)) }
def integrate (s1 s2 : SystemState) (r : Float := 0.5) : SystemState := { s1 with optionality := s1.optionality + r * s2.optionality }

def cycle (s s_ext : SystemState) : SystemState := integrate (repair (grind s)) s_ext

-- =========================================
-- RNG (LCG)
-- =========================================

def lcg_frac (seed : UInt32) : Float × UInt32 :=
  let a : UInt64 := 1664525
  let c : UInt64 := 1013904223
  let m : UInt64 := 4294967296
  let next := (a * seed.val + c) % m
  (next.toFloat / m.toFloat, next.toUInt32)

-- =========================================
-- Deterministic path
-- =========================================

def state_after_det (init : SystemState) (ext_seq : ℕ → SystemState) : ℕ → SystemState
  | 0     => init
  | n + 1 =>
      let prev := state_after_det init ext_seq n
      let ext  := ext_seq n
      let next_basin := gardener_intervention (update_basin prev.basin 0.84 0.68)
      cycle { prev with basin := next_basin } ext

-- =========================================
-- Adaptive agent path
-- =========================================

def agent_grind_factor (s : SystemState) : Float :=
  if s.optionality < 0.65  then 0.82
  else if s.optionality > 1.15 then 0.94
  else 0.90

def cycle_agent (s s_ext : SystemState) : SystemState :=
  integrate (repair (grind s (agent_grind_factor s))) s_ext

def state_after_agent (init : SystemState) (ext_seq : ℕ → SystemState) : ℕ → SystemState
  | 0     => init
  | n + 1 =>
      let prev := state_after_agent init ext_seq n
      let ext  := ext_seq n
      cycle_agent prev ext

def agent_utility (s : SystemState) : Float := s.optionality * 1.4 + s.invariants * 0.6

-- =========================================
-- Visual dashboard
-- =========================================

def level_marker (v : Nat) (low high : Nat) : String :=
  if v > high then "██" else if v > low then "█•" else "█ "

def visual_markers (b : Basin) : String :=
  let e := level_marker b.eggs         20 45
  let c := level_marker b.caterpillars 15 35
  let a := level_marker b.adults       10 28
  "\nLegend: █ = low   █• = medium   ██ = high\n" ++
  s!"  Eggs + : {e}     Cats C : {c}     Adults P : {a}\n" ++
  "  low     med     high\n" ++
  "  █       █•      ██"

def pp_state (n : ℕ) (s : SystemState) (extra : String := "") : String :=
  let b := s.basin
  s!"Cycle {n.toNat.padLeft 2 '0'}:  eggs={b.eggs.toNat.padLeft 4 ' '}  " ++
  s!"cats={b.caterpillars.toNat.padLeft 4 ' '}  adults={b.adults.toNat.padLeft 3 ' '}  " ++
  s!"viab={b.viability:.2f}  pred={b.predator_level:.2f}  " ++
  s!"inv={s.invariants:.2f}  opt={s.optionality:.2f}  budget={total_budget s:.2f}" ++
  (if extra ≠ "" then s!"   {extra}" else "") ++
  visual_markers b

-- =========================================
-- Example setup
-- =========================================

def initial_basin : Basin := ⟨55, 32, 18, 0.66, 0.14, []⟩
def initial_state : SystemState := ⟨initial_basin, 0.90, 0.58⟩

def ext_seq_example : ℕ → SystemState
  | 0 => ⟨⟨42, 24, 13, 0.59, 0.17, []⟩, 0.72, 0.44⟩
  | 1 => ⟨⟨50, 29, 17, 0.63, 0.13, []⟩, 0.79, 0.51⟩
  | n => ⟨⟨46 + n % 12, 26 + n % 9, 15 + n % 7, 0.61 + (n % 6).toFloat * 0.015, 0.15 - (n % 8).toFloat * 0.004, []⟩, 0.76, 0.49⟩

-- =========================================
-- Simulations
-- =========================================

def run_deterministic : List String :=
  (List.range 13).map fun n => pp_state n (state_after_det initial_state ext_seq_example n)

def run_adaptive : List String :=
  (List.range 13).map fun n =>
    let s := state_after_agent initial_state ext_seq_example n
    pp_state n s s!" util={agent_utility s:.2f}  grind={agent_grind_factor s:.2f}"

#eval run_deterministic
#eval run_adaptive
