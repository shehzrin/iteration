```{r}
nsduh_table_format = function(table_num,table_name) {
  
  out_table = 
    nsduh_html |> 
    html_table() |> 
    nth(table_num) |> 
    slice(-1) |> 
    mutate(drug = "table_name")
  
  return(out_table)
  
}

nsduh_table_format(html = nsduh_html, 1, "marj")
nsduh_table_format(html = nsduh_html, 2, "cocaine")
nsduh_table_format(html = nsduh_html, 3, "heroin")
```