!int_volume_insulate_core.f90
!      module int_volume_insulate_core
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_int_volume_insulate_core
!
      module int_volume_insulate_core
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_volume_insulate_core
!
      use calypso_mpi
      use m_machine_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use sum_volume_of_domain
!
      real(kind=kreal) :: vol_i_core_local
!
!
      if ( numele_in_core .gt. 0 ) then
!
        call sum_of_volume_by_ele_table(ele1%numele, ele1%interior_ele, &
     &      ele1%volume_ele, numele_in_core, iele_in_core_smp_stack,    &
     &      iele_in_core, vol_i_core_local)
!
        call MPI_allREDUCE(vol_i_core_local, inner_core%volume, ione,   &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
        if (my_rank.eq.0) write(*,*) inner_core%volume
!
      end if
!
      end subroutine s_int_volume_insulate_core
!
! ----------------------------------------------------------------------
!
      end module int_volume_insulate_core
