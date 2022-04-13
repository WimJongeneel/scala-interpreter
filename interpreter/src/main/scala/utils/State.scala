package utils

case class State[S, A](run: S => (S, A)) {

    def map[B](transform: A => B): State[S, B] = State(s0 => {
        val (s1, a) = run(s0)
        (s1, transform(a))
    })

    def bind[B](fromValue: A => State[S, B]): State[S, B] = State.join(map(fromValue))

    def put(state: S): State[S, A] = State(s0 => {
        val (_, a) = run(s0)
        (state, a)
    })

    def modify(update: S => S): State[S, A] = State(s0 => {
        val (s1, a) = run(s0)
        (update(s1), a)
    })

    def after(state: State[S, A]): State[S, A] = State(s0 => {
        val (s1, a) = run(s0)
        state.run(s1)
    })

    def before(state: State[S, A]): State[S, A] = State(s0 => {
        val (s1, a) = state.run(s0)
        run(s1)
    })

    def eval(state: S): A = run(state)._2

    def exec(state: S): S = run(state)._1
}

object State {

    def unit[S, A](value: A): State[S, A] = State(s => (s, value))

    def join[S, A](state: State[S, State[S, A]]): State[S, A] = State(s0 => {
        val (s1, state1) = state.run(s0)
        state1.run(s1)
    })
}