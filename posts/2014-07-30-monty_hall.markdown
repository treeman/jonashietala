---
layout: post
title: "Monty Hall"
tags: IDA Summer of Code, Rust
---

I'm currently reading [The Drunkard's Walk][], a well written book about probabilities.
There they discussed the famous [Monty Hall Problem][]. I've heard about it before
and I know the answer, but he mentioned a simulation of the problem and that sounded
cool so I made a simulation of my own. This example will be in the [rust][] documentation
as well.

Here's a quick summary of the problem:

> Suppose you're on a game show, and you're given the choice of three doors:
> Behind one door is a car; behind the others, goats. You pick a door, say No. 1,
> and the host, who knows what's behind the doors, opens another door, say No. 3,
> which has a goat. He then says to you, "Do you want to pick door No. 2?"
> Is it to your advantage to switch your choice?

The rather unintuitive answer is that you will have a 2/3 chance of winning if
you switch and a 1/3 chance of winning of you don't, so it's better to switch.

This program will simulate the game show and with large enough simulation steps
it will indeed confirm that it is better to switch.

[Monty Hall Problem]: http://en.wikipedia.org/wiki/Monty_Hall_problem "The Monty Hall Problem"
[rust]: http://www.rust-lang.org/ "rust"
[The Drunkard's Walk]: http://www.amazon.com/The-Drunkards-Walk-Randomness-Rules/dp/0307275175 "The Drunkard's Walk"

```{.rust}
use std::rand;
use std::rand::Rng;
use std::rand::distributions::{IndependentSample, Range};

struct SimulationResult {
    win: bool,
    switch: bool,
}

// Run a single simulation of the Monty Hall problem.
fn simulate<R: Rng>(random_door: &Range<uint>, rng: &mut R) -> SimulationResult {
    let car = random_door.ind_sample(rng);

    // This is our initial choice
    let mut choice = random_door.ind_sample(rng);

    // The game host opens a door
    let open = game_host_open(car, choice, rng);

    // Shall we switch?
    let switch = rng.gen();
    if switch {
        choice = switch_door(choice, open);
    }

    SimulationResult { win: choice == car, switch: switch }
}

// Returns the door the game host opens given our choice and knowledge of
// where the car is. The game host will never open the door with the car.
fn game_host_open<R: Rng>(car: uint, choice: uint, rng: &mut R) -> uint {
    let choices = free_doors(&[car, choice]);
    rand::sample(rng, choices.move_iter(), 1)[0]
}

// Returns the door we switch to, given our current choice and
// the open door. There will only be one valid door.
fn switch_door(choice: uint, open: uint) -> uint {
    free_doors(&[choice, open])[0]
}

fn free_doors(blocked: &[uint]) -> Vec<uint> {
    range(0u, 3).filter(|x| !blocked.contains(x)).collect()
}

fn main() {
    // The estimation will be more accurate with more simulations
    let num_simulations = 10000u;

    let mut rng = rand::task_rng();
    let random_door = Range::new(0u, 3);

    let (mut switch_wins, mut switch_losses) = (0u, 0u);
    let (mut keep_wins, mut keep_losses) = (0u, 0u);

    println!("Running {} simulations...", num_simulations);
    for _ in range(0, num_simulations) {
        let result = simulate(&random_door, &mut rng);

        match (result.win, result.switch) {
            (true, true) => switch_wins += 1,
            (true, false) => keep_wins += 1,
            (false, true) => switch_losses += 1,
            (false, false) => keep_losses += 1,
        }
    }

    let total_switches = switch_wins + switch_losses;
    let total_keeps = keep_wins + keep_losses;

    println!("Switched door {} times with {} wins and {} losses",
    total_switches, switch_wins, switch_losses);

    println!("Kept our choice {} times with {} wins and {} losses",
    total_keeps, keep_wins, keep_losses);

    // With a large number of simulations, the values should converge to
    // 0.667 and 0.333 respectively.
    println!("Estimated chance to win if we switch: {}",
    switch_wins as f32 / total_switches as f32);
    println!("Estimated chance to win if we don't: {}",
    keep_wins as f32 / total_keeps as f32);
}
```

This is an example run:

```
$ ./monty_hall
Running 10000 simulations...
Switched door 4994 times with 3323 wins and 1671 losses
Kept our choice 5006 times with 1619 wins and 3387 losses
Estimated chance to win if we switch: 0.665399
Estimated chance to win if we don't: 0.323412
```
