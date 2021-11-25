open Metro_network.Dijkstra

let main () =
  let eki = dijkstra "myogadani" "shinjuku-gyoemmae" in
  print_eki eki

let _ = main ()
