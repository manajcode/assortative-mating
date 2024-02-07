#re-evaluate binomial tests
# binomial test mating scheme 1
#chooser = female, starch
binom.test(x = 89, n = 196, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)

#binomial test mating scheme 2
# chosoer = female, CMY
binom.test(x = 113, n = 201, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)

#binomial test mating scheme 3
# chooser = male, CMY
binom.test(x = 106, n = 185, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)

#binomial test mating scheme 4
# chooser = male, starch 
binom.test(x = 88, n = 166, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)
