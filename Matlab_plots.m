
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Grab stress data for Hollow Sphere %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%{
clear all;
Stress_rr_array = csvread('Stress_rr_data.csv');
Stress_phiphi_array = csvread('Stress_phiphi_data.csv');
% read in M_rrr,sigma_m,sigma_eq,M_theta, seee porous file 
User_defined_data = csvread('UDDATA.csv');    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Length scale values from fortran %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
r_e = User_defined_data(1,1);
ratio = User_defined_data(1,2);
r_i = User_defined_data(1,3);
num1 = User_defined_data(1,4);
num2 = User_defined_data(1,5);
num3 = User_defined_data(1,6);
num4 = User_defined_data(1,7);

b1 = num2str(num1);
b2 = num2str(num2);
b3 = num2str(num3);
b4 = num2str(num4);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Extract Data for stress invariants %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Stress_rr_col1 = Stress_rr_array(:, 1);
Stress_rr_col2 = Stress_rr_array(:, 2);
Stress_rr_col3 = Stress_rr_array(:, 3);
Stress_rr_col4 = Stress_rr_array(:, 4);
Stress_rr_col5 = Stress_rr_array(:, 5);

Stress_phiphi_col1 = Stress_phiphi_array(:, 1);
Stress_phiphi_col2 = Stress_phiphi_array(:, 2);
Stress_phiphi_col3 = Stress_phiphi_array(:, 3);
Stress_phiphi_col4 = Stress_phiphi_array(:, 4);
Stress_phiphi_col5 = Stress_phiphi_array(:, 5);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Make Two Dimensional data plots %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure(1)
plot(Stress_rr_col1,Stress_rr_col2,'-','linewidth', 2); hold on;
plot(Stress_rr_col1,Stress_rr_col3, '+','linewidth',2); hold on;
plot(Stress_rr_col1,Stress_rr_col4, '--','linewidth',2); hold on;
plot(Stress_rr_col1,Stress_rr_col5, '*','linewidth',2); hold on
eq_title1 = '$$\sigma_{eq}$$';
eq_title2 = '$$r_e$$';
%title(eq_title,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
xlabel(eq_title2,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
ylabel(eq_title1,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')        
h = legend('0.50','0.52','0.54','0.56');
title(h, 'Internal radius values')
set(h, "Location", 'Best')
% Add the string containing the Latex expression to the plot
% This needs to be adjusted upon plot creation as a best fit algo 
%text(0.55, 240, 'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')      

figure(2)
plot(Stress_rr_col1,Stress_phiphi_col2,'*','linewidth',2); hold on;
plot(Stress_rr_col1,Stress_phiphi_col3, '+','linewidth',2); hold on;
plot(Stress_rr_col1,Stress_phiphi_col4, '--','linewidth',2); hold on;
plot(Stress_rr_col1,Stress_phiphi_col5, '*','linewidth',2)
eq_title1 = '$$r_e$$';
eq_title2 = '$$\sigma_{\theta \theta}$$';
%title(eq_title,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
xlabel(eq_title1,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
ylabel(eq_title2,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
h = legend('0.50','0.52','0.54','0.56');
title(h, 'Internal radius values')
set(h, "Location", 'Best')
% Add the string containing the Latex expression to the plot
% This needs to be adjusted upon plot creation as a best fit algo 
%text(0.55, -70 'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Three Dimensional Plots %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%}
Plots_3d = csvread('3d_data.csv');
x1 = Plots_3d(:, 1);
y1 = Plots_3d(:, 2);
z1 = Plots_3d(:, 3);

x2 = Plots_3d(:, 1);
y2 = Plots_3d(:, 2);
z2 = Plots_3d(:, 4);

x3 = Plots_3d(:, 1);
y3 = Plots_3d(:, 2);
z3 = Plots_3d(:, 5);

figure(3) 
scatter3(x1,y1,z1,'MarkerFaceColor','r','MarkerEdgeColor','b')
grid off
zlab = '$$\sigma_{rr}$$';
xlab = '$$r$$'
ylab = '$$b$$'
%title(zlab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
xlabel(xlab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
ylabel(ylab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
zlabel(zlab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')

figure(4)
scatter3(x2,y2,z2,'MarkerFaceColor','r','MarkerEdgeColor','b')
grid off
zlab = '$$\sigma_{\phi \phi}$$';
xlab = '$$r$$'
ylab = '$$b$$'
xlabel(xlab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
ylabel(ylab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
zlabel(zlab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')

figure(5)
scatter3(x3,y3,z3,'MarkerFaceColor','r','MarkerEdgeColor','b')
grid off
zlab = '$$M_{rrr} = -\frac{1}{2}M_{r\theta \theta}$$';
xlab = '$$r$$'
ylab = '$$b$$'
xlabel(xlab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
ylabel(ylab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
zlabel(zlab,'Interpreter', 'Latex', 'FontSize', 12, 'Color', 'k')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Porosity Equations   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
