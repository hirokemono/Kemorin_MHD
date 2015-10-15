!
!     module int_volume_of_domain
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on June, 2007
!
!      subroutine s_int_whole_volume_only(ele, jac_3d)
!      subroutine s_int_whole_volume_w_layer(ele, jac_3d, layer_tbl)
!      subroutine s_int_volume_of_domain(ele, jac_3d)
!
      module int_volume_of_domain
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_jacobians
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_whole_volume_only(ele, jac_3d)
!
      use sum_volume_of_domain
!
      type(jacobians_3d), intent(in) :: jac_3d
      type(element_data), intent(inout) :: ele
!
      call allocate_volume_4_smp
      call s_int_volume_of_domain(ele, jac_3d)
      call deallocate_volume_4_smp
!
      end subroutine s_int_whole_volume_only
!
!-----------------------------------------------------------------------
!
      subroutine s_int_whole_volume_w_layer(ele, jac_3d, layer_tbl)
!
      use t_layering_ele_list
      use sum_volume_of_domain
      use cal_layered_volumes
!
      type(jacobians_3d), intent(in) :: jac_3d
      type(element_data), intent(inout) :: ele
      type(layering_tbl), intent(inout) :: layer_tbl
!
      call allocate_volume_4_smp
!
      call s_int_volume_of_domain(ele, jac_3d)
      call s_cal_layered_volumes(ele, layer_tbl)
!
      call deallocate_volume_4_smp
!
      end subroutine s_int_whole_volume_w_layer
!
!-----------------------------------------------------------------------
!
      subroutine s_int_volume_of_domain(ele, jac_3d)
!
      use calypso_mpi
      use m_fem_gauss_int_coefs
      use fem_element_volume
      use sum_volume_of_domain
!
      type(jacobians_3d), intent(in) :: jac_3d
      type(element_data), intent(inout) :: ele
!
!
!      write(*,*) 'fem_element_volume_pg', max_int_point
       call fem_element_volume_pg(ele, jac_3d, max_int_point)
!
!     ---  lead total volume
!
!      write(*,*) 'sum_4_volume'
      call sum_4_volume(ele%numele, ele%interior_ele,                   &
     &    ele%istack_ele_smp, ele%volume_ele, vol_local)
!
!      write(*,*) 'MPI_allREDUCE'
       call MPI_allREDUCE (vol_local, ele%volume, ione,                 &
     &  CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
       if (ele%volume .eq. 0.0d0) then
         ele%a_vol = 1.0d30
       else
         ele%a_vol = 1.0d0 / ele%volume
       end if
!
       end subroutine s_int_volume_of_domain
!
!-----------------------------------------------------------------------
!
      end module int_volume_of_domain
