---
title: "Jumping into Elixir during a one-week hackathon"
tags: Coinparty, Cryptocurrency
---

# The cards stacked against us

I wanted to use Elixir to create a 


* Both of us had other commitments so we couldn't focus on this full-time.
* I had some experience with Elixir and Phoenix, but had never made anything real with them.
* While we both had a high level understanding of cryptocurrencies, we've never programmed against them.

# Phoenix LiveView is great

One of the great things about Phoenix is LiveView. It gives you the ability to write websites that updates the page in real-time but without having to write any JavaScript, so all your templates and logic can be handled in the same manner on the server.

This doesn't fully replace JavaScript or front-end frameworks, but you can get very far very quickly. This is something I felt when I implemented the UI transitions for a payment, in the first few hours of the hackathon and without any prior experience with LiveView.

I basically wanted to implement a responsive UI for accepting a Bitcoin Cash payment, going through these transitions:

![The UI transitions. (In the end it's a bit more complicated but this is what I started with.)](/images/coinparty/payment-flow.svg)

It doesn't really matter what they mean, just that we should move from one to another when we receive a trigger of some sort. Simulated with timers this is how it looked like:

![](/images/coinparty/payment.gif)

What I started with was trying to render the different states. This could be done with regular Phoenix views, and called dynamically depending on which state you're in:

```{.elixir}
defmodule Demo.PaymentLive do
  use Demo, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, state: :setup)}
  end

  @impl true
  def render(assigns = %{state: state}) do
    template = Atom.to_string(state) <> ".html"
    render_existing(Demo.PaymentView, template, assigns)
  end
```

You begin in state `:setup` and you'll render the `setup.html` template via the `Demo.PaymentView` view.

After the setup, where you'll enter the email and amount you want to pay, we update the socket with the new state:

```{.elixir}
  @impl true
  def handle_event("setup", %{"setup" => %{"email" => email,
                                           "amount" => amount}}, socket) do
    address = "bitcoincash:qqpkcce4lzdc8guam5jfys9prfyhr90seqzakyv4tu"

    {:noreply,
     assign(socket,
            state: :wait_for_tx, # Change state here
            email: email,
            amount: amount,
            address: address)}
  end
```

And we have a responsive webpage! LiveView will notice we've changed the socket state, render `wait_for_tx.html` and serve the diff to the client.

All that's left is to simulate the other state transitions, and a simple timer will do:

```{.elixir}
  @impl true
  def handle_event("setup", %{"setup" => %{"email" => email,
                                           "amount" => amount}}, socket) do
    address = "bitcoincash:qqpkcce4lzdc8guam5jfys9prfyhr90seqzakyv4tu"

    # Simulate success after 2s
    :timer.send_after(2000, self(), :tx_seen)

    {:noreply,
     assign(socket,
            state: :wait_for_tx,
            email: email,
            amount: amount,
            address: address)}
  end
```

This will notify `handle_info` (as it's a GenServer), and we can continue our transition from there:

```{.elixir}
  @impl true
  def handle_info(:tx_seen, socket) do
      # Simulate a confirmation after 1s
      :timer.send_after(1000, self(), :new_block)

      {:noreply, assign(socket, state: :wait_for_confirmations, confirmations: 0)}
    end
  end
```

And the UI is basically complete!

I managed to figure this out with little experience with Elixir, Phoenix, LiveView or Genservers, quite quickly, which I think is a testament to how easy it is to work with.



# Other things I liked

1. Functional programming and immutability
2. Processes
3. LSP integration with Neovim worked great
4. mix

# Things I missed

1. Static typing, and especially enums

# Closing thoughts


