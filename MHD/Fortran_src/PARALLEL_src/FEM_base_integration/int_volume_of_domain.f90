!
!     module int_volume_of_domain
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on June, 2007
!
!      subroutine s_int_whole_volume_only
!      subroutine s_int_whole_volume_w_layer
!      subroutine s_int_volume_of_domain
!
      module int_volume_of_domain
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_whole_volume_only
!
      use sum_volume_of_domain
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain
      call deallocate_volume_4_smp
!
      end subroutine s_int_whole_volume_only
!
!-----------------------------------------------------------------------
!
      subroutine s_int_whole_volume_w_layer
!
      use sum_volume_of_domain
      use cal_layered_volumes
!
      call allocate_volume_4_smp
!
      call s_int_volume_of_domain
      call s_cal_layered_volumes
!
      call deallocate_volume_4_smp
!
      end subroutine s_int_whole_volume_w_layer
!
!-----------------------------------------------------------------------
!
      subroutine s_int_volume_of_domain
!
      use calypso_mpi
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use fem_element_volume
      use sum_volume_of_domain
!
!
!      write(*,*) 'fem_element_volume_pg', max_int_point
       call fem_element_volume_pg(max_int_point)
!
!     ---  lead total volume
!
!      write(*,*) 'sum_4_volume'
      call sum_4_volume(ele1%numele, ele1%interior_ele,                 &
     &    ele1%istack_ele_smp, ele1%volume_ele, vol_local)
!
!      write(*,*) 'MPI_allREDUCE'
       call MPI_allREDUCE (vol_local, ele1%volume, ione,                 &
     &  CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
       if (ele1%volume .eq. 0.0d0) then
         ele1%a_vol = 1.0d30
       else
         ele1%a_vol = 1.0d0 / ele1%volume
       end if
!
       end subroutine s_int_volume_of_domain
!
!-----------------------------------------------------------------------
!
      end module int_volume_of_domain
