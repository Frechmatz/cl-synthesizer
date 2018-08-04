% Matlab Script that plots exponential characteristic of VCA-Core
maxCV = 10.0;
cv = 0:0.01:maxCV;
y = (pow2 (cv) - 1.0) / pow2(maxCV);
plot (cv,y);


