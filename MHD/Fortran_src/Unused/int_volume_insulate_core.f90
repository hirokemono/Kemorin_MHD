!int_volume_insulate_core.f90
!      module int_volume_insulate_core
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine s_int_volume_insulate_core(ele, inner_core)
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: inner_core
!
      module int_volume_insulate_core
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data_MHD
      use t_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_volume_insulate_core(ele, inner_core)
!
      use calypso_mpi
      use sum_volume_of_domain
!
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: inner_core
!
      real(kind=kreal) :: vol_i_core_local
!
!
      if ( inner_core%numele_fld .eq. 0 ) return
!
      call sum_of_volume_by_ele_table(ele%numele, ele%interior_ele,     &
     &    ele%volume_ele, inner_core%numele_fld,                        &
     &    inner_core%istack_ele_fld_smp, inner_core%iele_fld,           &
     &    vol_i_core_local)
!
      call MPI_allREDUCE(vol_i_core_local, inner_core%volume, ione,     &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (my_rank.eq.0) write(*,*) inner_core%volume
!
      end subroutine s_int_volume_insulate_core
!
! ----------------------------------------------------------------------
!
      end module int_volume_insulate_core
