use feature qw(say);

while (<>) {
  unless (2 != $. % 4) {
    $freq{length()-1}++;
  }
}

foreach (sort keys %freq) {
  say "$freq{$_} $_";
}
