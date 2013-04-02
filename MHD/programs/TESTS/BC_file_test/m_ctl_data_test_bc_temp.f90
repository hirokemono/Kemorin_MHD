!m_ctl_data_test_bc_temp.f90
!      module m_ctl_data_test_bc_temp
!
!      Written by H. Matsui on July, 2006
!      Mmodified by H. Matsui on June, 2007
!
!      subroutine read_control_4_bc_temp
!
!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_subdomain_ctl           2
!!      num_smp_ctl                 1
!!  
!!      mesh_file_head_ctl       'mesh/in'
!!      mesh_file_fmt_ctl        'gzip'
!!    end data_files_def
!!  
!!    begin boundary_ctl
!!      node_grp_name_ctl      'CMB'
!!      hermonics_degree_ctl      2
!!      hermonics_order_ctl      -2
!!    end boundary_ctl
!!  end mesh_test
!!
!!    -------------------------------------------------------------------
!
      module m_ctl_data_test_bc_temp
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), parameter :: test_mest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_bc_temp"
!
      character(len = kchara) :: temp_nod_grp_name
      integer(kind = kint) :: hermonic_degree_ctl = 1
      integer(kind = kint) :: hermonic_order_ctl =  0
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_mesh_test_ctl = 'mesh_test'
      integer (kind=kint) :: i_mesh_test_ctl = 0
!
!     1st level
!
      character(len=kchara), parameter :: hd_bc_def =   'boundary_ctl'
      integer (kind=kint) :: i_bc_def =    0
!
!     2nd level for boundary defeine
!
      character(len=kchara), parameter                                  &
     &                      :: hd_nod_grp_t =  'node_grp_name_ctl'
      character(len=kchara), parameter                                  &
     &                      :: hd_sph_degree = 'hermonics_degree_ctl'
      character(len=kchara), parameter                                  &
     &                      :: hd_sph_order =  'hermonics_order_ctl'
      integer (kind=kint) :: i_nod_grp_t =    0
      integer (kind=kint) :: i_sph_degree =   0
      integer (kind=kint) :: i_sph_order =    0
!
      private :: test_mest_ctl_file_code, fname_test_mesh_ctl
      private :: hd_bc_def, i_bc_def
      private :: read_test_mesh_ctl_data
      private :: hd_nod_grp_t, hd_sph_degree, hd_sph_order
!
      private :: read_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_bc_temp
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = test_mest_ctl_file_code
!
      open(ctl_file_code, file = fname_test_mesh_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_test_mesh_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_bc_temp
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_test_mesh_ctl_data
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_mesh_test_ctl) .eq. 0) return
      if (i_mesh_test_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_mesh_test_ctl, i_mesh_test_ctl)
        if(i_mesh_test_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_temp_nod_bc
      end do
!
      end subroutine read_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_4_temp_nod_bc
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_bc_def) .eq. 0) return
      if (i_bc_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_bc_def, i_bc_def)
        if(i_bc_def .gt. 0) exit
!
!
        call read_character_ctl_item(hd_nod_grp_t,                      &
     &        i_nod_grp_t, temp_nod_grp_name)
!
        call read_integer_ctl_item(hd_sph_degree,                       &
     &        i_sph_degree, hermonic_degree_ctl)
        call read_integer_ctl_item(hd_sph_order,                        &
     &        i_sph_order, hermonic_order_ctl)
      end do
!
      end subroutine read_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_test_bc_temp
