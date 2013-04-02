!
!     module   m_file_control_parameter
!.......................................................................
!
!     Written by H. Matsui & H. Okuda
!     Modified by H. Matsui  on  Aug., 2007
!
      module   m_file_control_parameter
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint), parameter :: control_file_code = 11
!
      integer(kind=kint), parameter :: neib_file_code =     19
!
      integer(kind=kint), parameter :: time_step_data_code = 49
      integer(kind=kint), parameter :: rms_data_code =       48
! 
      integer(kind=kint), parameter :: maximum_data_code =     44
      integer(kind=kint), parameter :: maximum_position_code = 45
! 
      integer(kind=kint), parameter :: boundary_monitor_code = 46
! 
      integer(kind=kint), parameter :: id_monitor_file = 47
! 
      integer(kind=kint), parameter :: sgs_fld_coef_file_code =  21
      integer(kind=kint), parameter :: sgs_comp_coef_file_code = 22
      integer(kind=kint), parameter :: diff_coef_file_code =  23
!
      integer(kind=kint), parameter :: sgs_cor_file_code =    24
      integer(kind=kint), parameter :: sgs_ratio_file_code =  25
      integer(kind=kint), parameter :: sgs_rms_file_code =    26
      integer(kind=kint), parameter :: diff_cor_file_code =   27
      integer(kind=kint), parameter :: diff_ratio_file_code = 28
      integer(kind=kint), parameter :: diff_rms_file_code =   29
!
      integer(kind=kint), parameter :: sgs_fld_whole_file_code =  31
      integer(kind=kint), parameter :: sgs_comp_whole_file_code = 32
      integer(kind=kint), parameter :: diff_fld_whole_file_code = 33
      integer(kind=kint), parameter :: diff_comp_whole_file_code = 134
!
      integer(kind=kint), parameter :: sgs_w_cor_file_code =    34
      integer(kind=kint), parameter :: sgs_w_ratio_file_code =  35
      integer(kind=kint), parameter :: sgs_w_rms_file_code =    36
      integer(kind=kint), parameter :: diff_w_cor_file_code =   37
      integer(kind=kint), parameter :: diff_w_ratio_file_code = 38
      integer(kind=kint), parameter :: diff_w_rms_file_code =   39
!
      integer(kind=kint), parameter :: sgs_diff_max_code =   30
!
      integer(kind=kint), parameter :: dt_check_max_code =   15
      integer(kind=kint), parameter :: dt_check_min_code =   17
!
      integer(kind=kint), parameter :: id_dynamobench = 41
!
      character(len=kchara) :: time_step_file
      character(len=kchara) :: time_step_data_file
! 
      character(len=kchara) :: rms_data_file
! 
      character(len=kchara) :: nod_monitor_file_name
! 
      character(len=kchara) :: neib_file_name
!
      character(len=kchara), parameter                                  &
     &       :: volume_ave_file_name =     'time_step_data.dat'
      character(len=kchara), parameter                                  &
     &       :: volume_rms_file_name =     'time_rms_data.dat'
!
      character(len=kchara), parameter                                  &
     &       :: minmax_data_file_name =     'maximum_data.dat'
      character(len=kchara), parameter                                  &
     &       :: minmax_posi_file_name =     'maximum_posi.dat'
!
      character(len=kchara), parameter                                  &
     &       :: sgs_fld_coef_file_name =  'sgs_model_coefs.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_comp_coef_file_name = 'sgs_model_coefs_comp.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_fld_whole_file_name =  'sgs_m_coefs_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_comp_whole_file_name = 'sgs_m_coefs_whole_comp.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_coef_file_name =    'diff_model_coefs.dat'
      character(len=kchara), parameter  :: diff_fld_whole_file_name     &
     &        = 'diff_m_coefs_whole.dat'
      character(len=kchara), parameter  :: diff_comp_whole_file_name    &
     &        = 'diff_m_comp_coefs_whole.dat'
!
      character(len=kchara), parameter                                  &
     &       :: sgs_cor_file_name =      'sgs_correlate.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_ratio_file_name =    'sgs_rms_ratio.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_rms_file_name =      'sgs_rms.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_cor_file_name =     'diff_correlate.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_ratio_file_name =   'diff_rms_ratio.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_rms_file_name =     'diff_rms.dat'
!
      character(len=kchara), parameter                                  &
     &       :: sgs_w_cor_file_name =    'sgs_correlate_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_w_ratio_file_name =  'sgs_rms_ratio_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: sgs_w_rms_file_name =    'sgs_rms_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_w_cor_file_name =   'diff_correlate_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_w_ratio_file_name = 'diff_rms_ratio_whole.dat'
      character(len=kchara), parameter                                  &
     &       :: diff_w_rms_file_name =   'diff_rms_whole.dat'
!
      character(len=kchara), parameter                                  &
     &      :: sgs_diff_max_name =       'sgs_differences.dat'
!
      character(len=kchara), parameter                                  &
     &      :: dt_check_max_name = 'maximum_dt_chack.dat'
      character(len=kchara), parameter                                  &
     &      :: dt_check_min_name = 'minimum_dt_chack.dat'
!
      character(len=kchara), parameter :: node_monitor_head = 'node'
!
      character(len=kchara), parameter                                  &
     &      :: dynamobench_field_name = 'dynamobench_field.dat'
!
      end module m_file_control_parameter
