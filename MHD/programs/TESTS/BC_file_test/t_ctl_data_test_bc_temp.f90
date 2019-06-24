!t_ctl_data_test_bc_temp.f90
!      module t_ctl_data_test_bc_temp
!
!      Written by H. Matsui on July, 2006
!      Mmodified by H. Matsui on June, 2007
!
!!      subroutine read_control_4_bc_temp(bc_temp_test_ctl)
!!      subroutine reset_test_mesh_ctl_data(bc_temp_test_ctl)
!!        type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_subdomain_ctl           2
!!      num_smp_ctl                 1
!!  
!!      mesh_file_prefix         'mesh/in'
!!      mesh_file_fmt_ctl        'gzip'
!!    end data_files_def
!!  
!!    begin boundary_ctl
!!      node_grp_name_ctl      'CMB'
!!      harmonics_degree_ctl      2
!!      harmonics_order_ctl      -2
!!    end boundary_ctl
!!  end mesh_test
!!
!!    -------------------------------------------------------------------
!
      module t_ctl_data_test_bc_temp
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_control_elements
!
      implicit  none
!
!
      integer(kind = kint), parameter :: test_mest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_bc_temp"
!
      type ctl_data_bc_temp_test
!>      Structure for file settings
        type(platform_data_control) :: bc_test_plt
!
        type(read_character_item) :: temp_nod_grp_name
        type(read_integer_item) :: hermonic_degree_ctl
        type(read_integer_item) :: hermonic_order_ctl
!
        integer (kind=kint) :: i_mesh_test_ctl = 0
        integer (kind=kint) :: i_bc_def =    0
      end type ctl_data_bc_temp_test
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_mesh_test_ctl = 'mesh_test'
!
!     1st level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter :: hd_bc_def =   'boundary_ctl'
!
!     2nd level for boundary defeine
!
      character(len=kchara), parameter                                  &
     &                      :: hd_nod_grp_t =  'node_grp_name_ctl'
      character(len=kchara), parameter                                  &
     &                      :: hd_sph_degree = 'harmonics_degree_ctl'
      character(len=kchara), parameter                                  &
     &                      :: hd_sph_order =  'harmonics_order_ctl'
!
      private :: test_mest_ctl_file_code, fname_test_mesh_ctl
!
      private :: hd_platform, hd_bc_def
      private :: hd_nod_grp_t, hd_sph_degree, hd_sph_order
      private :: read_test_mesh_ctl_data
      private :: bcast_test_mesh_ctl_data
!
      private :: read_ctl_data_4_temp_nod_bc
      private :: bcast_ctl_data_4_temp_nod_bc
      private :: reset_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_bc_temp(bc_temp_test_ctl)
!
      use skip_comment_f
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(test_mest_ctl_file_code, file = fname_test_mesh_ctl,       &
     &       status='old')
        do
          call load_one_line_from_control                               &
     &       (test_mest_ctl_file_code, c_buf1)
          call read_test_mesh_ctl_data(test_mest_ctl_file_code,         &
     &        hd_mesh_test_ctl, bc_temp_test_ctl, c_buf1)
          if(bc_temp_test_ctl%i_mesh_test_ctl .gt. 0) exit
        end do
        close(test_mest_ctl_file_code)
      end if
!
      call bcast_test_mesh_ctl_data(bc_temp_test_ctl)
!
      end subroutine read_control_4_bc_temp
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_test_mesh_ctl_data                                &
     &         (id_control, hd_block, bc_temp_test_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(bc_temp_test_ctl%i_mesh_test_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms(id_control, hd_platform,            &
     &      bc_temp_test_ctl%bc_test_plt, c_buf)
        call read_ctl_data_4_temp_nod_bc                                &
     &     (id_control, hd_bc_def, bc_temp_test_ctl, c_buf)
      end do
      bc_temp_test_ctl%i_mesh_test_ctl = 1
!
      end subroutine read_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_test_mesh_ctl_data(bc_temp_test_ctl)
!
      use bcast_4_platform_ctl
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!
!
      call bcast_ctl_data_4_temp_nod_bc(bc_temp_test_ctl)
      call bcast_ctl_data_4_platform(bc_temp_test_ctl%bc_test_plt)
!
      call MPI_BCAST(bc_temp_test_ctl%i_mesh_test_ctl, 1,               &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_test_mesh_ctl_data(bc_temp_test_ctl)
!
      use skip_comment_f
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!
!
      call reset_ctl_data_4_temp_nod_bc(bc_temp_test_ctl)
      call reset_control_platforms(bc_temp_test_ctl%bc_test_plt)
!
      bc_temp_test_ctl%i_mesh_test_ctl = 0
!
      end subroutine reset_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_4_temp_nod_bc                            &
     &         (id_control, hd_block, bc_temp_test_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(bc_temp_test_ctl%i_bc_def .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_nod_grp_t, bc_temp_test_ctl%temp_nod_grp_name)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sph_degree, bc_temp_test_ctl%hermonic_degree_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sph_order, bc_temp_test_ctl%hermonic_order_ctl)
      end do
      bc_temp_test_ctl%i_bc_def = 1
!
      end subroutine read_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_temp_nod_bc(bc_temp_test_ctl)
!
      use bcast_control_arrays
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!
!
      call bcast_ctl_type_c1(bc_temp_test_ctl%temp_nod_grp_name)
      call bcast_ctl_type_i1(bc_temp_test_ctl%hermonic_degree_ctl)
      call bcast_ctl_type_i1(bc_temp_test_ctl%hermonic_order_ctl)
!
      call MPI_BCAST(bc_temp_test_ctl%i_bc_def, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      subroutine reset_ctl_data_4_temp_nod_bc(bc_temp_test_ctl)
!
      type(ctl_data_bc_temp_test), intent(inout) :: bc_temp_test_ctl
!
!
      bc_temp_test_ctl%temp_nod_grp_name%iflag = 0
      bc_temp_test_ctl%hermonic_degree_ctl%iflag = 0
      bc_temp_test_ctl%hermonic_order_ctl%iflag = 0
!
      bc_temp_test_ctl%i_bc_def = 0
!
      end subroutine reset_ctl_data_4_temp_nod_bc
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_test_bc_temp
