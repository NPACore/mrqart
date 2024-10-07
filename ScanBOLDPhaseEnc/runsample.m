%
% Matlab example to call 'runscan.m'
%

% inputs
project = 'WPC-7366';
%StartYr = 2018; EndYr = 2019;
%StartDate = '0101'; EndDate = '0631';
StartYr = 2021; EndYr = 2022;
StartDate = '0101'; EndDate = '1231';

project = 'WPC-7350';
StartYr = 2024; EndYr = 2024;
StartDate = '0101'; EndDate = '1231';
StartDate = '0801'; EndDate = '0830';

% runscan
runscan(project, StartYr,EndYr, StartDate, EndDate);
