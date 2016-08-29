## Double checking rate calculations


set.seed(1232)
windows(9,9)
par(mfrow = c(3,3))
outcome.rate('aiputamen', data = Imaging, plot.random = 9)
set.seed(1232)
windows(9,9)
par(mfrow = c(3,3))
outcome.rate('aiputamen', data = Imaging, plot.random = 9, allow_int = F)


outcome.rate('countdensityratio', data = Imaging, plot.random = 9, baseline.id = 'SC')
outcome.rate('meanstriatum', data = Imaging, plot.random = 9, baseline.id = 'SC')



outcome.rate('aiputamen', scale = 'relative', data = Imaging, plot.random = 9, baseline.id = 'SC')
outcome.rate('countdensityratio', scale = 'relative', data = Imaging, plot.random = 9, baseline.id = 'SC')
outcome.rate('meanstriatum', scale = 'relative', data = Imaging, plot.random = 9, baseline.id = 'SC')
