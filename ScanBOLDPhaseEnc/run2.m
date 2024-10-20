%
% Extract EPI core parameters from project/study/dicom files
% @chm - 07/08/2021
%


%%
clear all;

%% add path
addpath('/raidgyrus2/users/moonc/SoftwareDevelop/ScanDiffTable');

%% folder of data archive
%{
pfolderlist = {...
    '/disk/mace2/archive/scan_data_archive/2015',...
    '/disk/mace2/archive/scan_data_archive/2016',...
    '/disk/mace2/archive/scan_data_archive/2017',...
    '/disk/mace2/archive/scan_data_archive/2017_2',...
    '/disk/mace2/archive/scan_data_archive/2018',...
    '/disk/mace2/archive/scan_data_archive/2019',...
    '/disk/mace2/scan_data',...
};
%}
pfolderlist = {...
    '/disk/mace2/scan_data',...
};
%    '/disk/mace2/archive/scan_data_archive/2019',...
%    '/disk/mace2/archive/scan_data_archive/2020',...
%    '/disk/mace2/archive/scan_data_archive/2021',...
%    '/disk/mace2/scan_data',...
%/disk/mace2/archive/scan_data_archive/2021/WPC-7366

%rangestudydate = ['20190101'; '20191231'];
%rangestudydate = ['20200101'; '20210830'];
%rangestudydate = ['20210710'; '20210730'];
%rangestudydate = ['20160101'; '20210830'];
%rangestudydate = ['20160101'; '20181231'];
%rangestudydate = ['20180101'; '20181231'];
%rangestudydate = ['20190101'; '20191231'];
%rangestudydate = ['20200101'; '20201231'];
%rangestudydate = ['20210101'; '20211231'];
rangestudydate = ['20220101'; '20221231'];
%rangestudydate = [];

%% Porject name to be scanned
%{
projectlist = {...
    'WPC-7091',...
    'WPC-8007',...
    'WPC-7999',...
};
%}
projectlist = {...
    %'WPC-8076',...
    %'WPC-8291',...
    %'WPC-9919',...
    %'WPC-8242',...
    %'WPC-6130',...
    %'WPC-7365',...
    %'OKO-BHEALTH',...
    %'RES-UUI',...
    %'RES-NORM',...
    %'WPC-7366',...
    %'WPC-7642',...
    %'WPC-6106',...
    %'WPC-8242',...
    %'WPC-7860',...
    %'WPC-8287',...
    %'WPC-8117',...
    %'WPC-7708',...
    %'WPC-8291',...
    %'WPC-8076',...
    %'WPC-7674',...
    %'ROS-MOVE',...
    'WPC-7366',...
    };
nproject = length(projectlist);

%@chm - 2022/11/16
% ;; Not used!
EPItype = 'BOLD'; %'BOLD'; %'DWI'

%% run
for iproj=1:nproject
    project = projectlist{1,iproj};
    [tableLine,projectfilename] = extractepiped_func2(project, pfolderlist,rangestudydate, EPItype);
    
    decisionB0ShimSuccess(tableLine,projectfilename);
end

%% remove path
rmpath('/raidgyrus2/users/moonc/SoftwareDevelop/ScanDiffTable');



    