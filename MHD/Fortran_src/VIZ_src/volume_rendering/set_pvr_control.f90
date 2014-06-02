!
!      module set_pvr_control
!
!     Written by H. Matsui on May., 2006
!
!      subroutine s_set_pvr_control(num_mat, mat_name,                  &
!     &          num_nod_phys, phys_nod_name, ierr)
!
      module set_pvr_control
!
      use m_precision
!
      use m_control_data_pvrs
      use m_control_data_4_pvr
      use m_control_params_4_pvr
!
      implicit none
!
      private :: read_control_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_control(num_mat, mat_name,                   &
     &          num_nod_phys, phys_nod_name, ierr)
!
      use set_control_each_pvr
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: i
!
!
      ctl_file_code = pvr_ctl_file_code
!
      call allocate_ctl_param_4_pvr
      call allocate_pvr_ctl_struct
!
      do i = 1, num_pvr
        call read_control_pvr(i, ierr)
      end do
!
      do i = 1, num_pvr
        call count_control_pvr(i, pvr_ctl_struct(i), num_mat, mat_name, &
     &      num_nod_phys, phys_nod_name)
      end do
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_components_4_pvr',    &
     &         num_pvr
      call allocate_components_4_pvr
!
      do i = 1, num_pvr
        if(iflag_debug .gt. 0) write(*,*) 'set_control_pvr',i
        call set_control_pvr(i, pvr_ctl_struct(i), num_mat, mat_name,   &
     &      num_nod_phys, phys_nod_name)
        if(iflag_debug .gt. 0) write(*,*) 'deallocate_cont_dat_pvr',i
        call deallocate_cont_dat_pvr(pvr_ctl_struct(i))
     end do
!
      call allocate_loght_posi_in_view
      call deallocate_pvr_file_header_ctl
!
      end subroutine s_set_pvr_control
!
!   --------------------------------------------------------------------
!
      subroutine read_control_pvr(i_pvr, ierr)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(inout) :: ierr
!
!
!
      if(fname_pvr_ctl(i_pvr) .eq. 'NO_FILE') return
!
      call reset_pvr_control_flags(pvr_ctl_struct(i_pvr))
      if(my_rank .eq. 0) write(*,*) 'PVR control:', i_pvr,':  ',        &
     &                      trim( fname_pvr_ctl(i_pvr) )
!
      open(pvr_ctl_file_code, file=fname_pvr_ctl(i_pvr), status='old')
      call read_control_data_pvr(pvr_ctl_struct(i_pvr), ierr)
      close(pvr_ctl_file_code)
!
      end subroutine read_control_pvr
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_control
