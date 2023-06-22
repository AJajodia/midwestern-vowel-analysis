ggplot()+
  geom_point(data = mathias_graph, aes(x = F2.-F1., y = F1.), color = "aquamarine") +
  geom_text(aes(x = F2.-F1.+45, y = F1.), label = mathias_graph$Vowel, data = mathias_graph)+
  geom_point(data = men_labov_graph, aes(x = F2.-F1., y = F1., label = Vowel), color = "deepskyblue")+
  geom_text(aes(x = F2.-F1.+45, y = F1.), label = men_labov_graph$Vowel, data = men_labov_graph)+
  scale_x_reverse()+scale_y_reverse()

men_labov_graph["Vowel"] = IPA

ggplot()+
  geom_point(data = isabel_graph, aes(x = F2.-F1., y = F1.), color = "magenta") +
  geom_text(aes(x = F2.-F1.+45, y = F1.), label = isabel_graph$Vowel, data = isabel_graph)+
  geom_point(data = women_labov_graph, aes(x = F2.-F1., y = F1., label = Vowel), color = "red")+
  geom_text(aes(x = F2.-F1.+45, y = F1.), label = women_labov_graph$Vowel, data = women_labov_graph)+
  scale_x_reverse()+scale_y_reverse()

