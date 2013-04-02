!
!      module m_ctl_data_gen_z_filter
!
      module m_ctl_data_gen_z_filter
!
!      Written by H. Matsui on July, 2006
!
      use m_precision
      use m_ctl_data_4_solvers
      use m_ctl_data_gen_filter
!
      implicit  none
!
!
      integer(kind = kint), parameter :: filter_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_zfilter_ctl = "ctl_z_filter"
!
      character(len = kchara) :: filter_head_ctl = "filter_node_l"
!
      integer(kind = kint) :: ip_smp_p_ctl = 1
!
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_control = 'const_z_filter'
      integer (kind=kint) :: i_filter_control = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_head_ctl =  'filter_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_ip_smp_p_ctl =     'num_smp_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_plane_ctl =        'domain_param_ctl'
      integer (kind=kint) :: i_filter_head_ctl =  0
      integer (kind=kint) :: i_ip_smp_p_ctl =   0
      integer (kind=kint) :: i_plane_ctl = 0
!
!
!  ---------------------------------------------------------------------
!
!      contains
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_gen_z_filter
