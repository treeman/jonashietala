---
title: "Coinparty retrospective"
tags: Coinparty, Cryptocurrency 
---

# Phoenix LiveView is great

One of the big benefits of Phoenix is LiveView: the ability 

![](/images/coinparty/payment.gif)

```{.elixir}
defmodule Demo.PaymentLive do
  use Demo, :live_view

  @required_confirmations 3

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, state: :setup, required_confirmations: @required_confirmations)}
  end

  @impl true
  def render(assigns = %{state: state}) do
    template = Atom.to_string(state) <> ".html"
    render_existing(Demo.PaymentView, template, assigns)
  end

  @impl true
  def handle_event("setup", %{"setup" => %{"email" => email, "amount" => amount}}, socket) do
    # FIXME ensure email/amount are correct

    # FIXME get a new address from payments backend
    address = "bitcoincash:qqpkcce4lzdc8guam5jfys9prfyhr90seqzakyv4tu"

    # Simulate success after 2s
    :timer.send_after(2000, self(), :tx_seen)

    {:noreply, assign(socket, state: :wait_for_tx, email: email, amount: amount, address: address)}
  end

  @impl true
  def handle_info(:tx_seen, socket) do
    if socket.assigns.required_confirmations == 0 do
      # Simulate acceptance after 2s
      :timer.send_after(2000, self(), :accepted)

      {:noreply, assign(socket, state: :wait_for_verification)}
    else
      # Simulate a confirmation after 1s
      :timer.send_after(1000, self(), :new_block)

      {:noreply, assign(socket, state: :wait_for_confirmations, confirmations: 0)}
    end
  end

  # FIXME handle double-spend info messages, should goto :denied

  @impl true
  def handle_info(:new_block, socket) do
    # FIXME need to confirm that the transaction is in the block, otherwise goto :denied

    confirmations = socket.assigns.confirmations + 1
    if confirmations >= socket.assigns.required_confirmations do
      {:noreply, assign(socket, state: :accepted, confirmations: confirmations)}
    else
      :timer.send_after(1000, self(), :new_block)
      {:noreply, assign(socket, confirmations: confirmations)}
    end
  end

  @impl true
  def handle_info(:accepted, socket) do
    # FIXME send a confirmation email, if it exists

    {:noreply, assign(socket, state: :accepted)}
  end
end

```
