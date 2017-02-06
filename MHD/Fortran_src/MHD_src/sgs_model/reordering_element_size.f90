!
!      module reordering_element_size
!
!      Written by H. Matsui on Nov., 2006
!      Modified by H. Matsui on Feb., 2008
!
!!      subroutine reordering_ele_size(SGS_param, numele)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!      subroutine reordering_scalar_by_layer
!!      subroutine reordering_vector_by_layer(numele, old2newele, elen)
!!      subroutine reordering_layer_info(numele, old2newele_layer,      &
!!     &          n_item_layer_d, item_layer)
!
      module reordering_element_size
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: dx_ordering_tmp(:)
      private :: dx_ordering_tmp
      private :: allocate_dx_ordering_tmp, deallocate_dx_ordering_tmp
      private :: reordering_scalar_by_layer, reordering_vector_by_layer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_dx_ordering_tmp(numele)
!
      integer(kind = kint), intent(in) :: numele
!
      allocate( dx_ordering_tmp(numele) )
      dx_ordering_tmp = 0.0d0
!
      end subroutine allocate_dx_ordering_tmp
!
!------------------------------------------------------------------
!
      subroutine deallocate_dx_ordering_tmp
!
      deallocate( dx_ordering_tmp )
!
      end subroutine deallocate_dx_ordering_tmp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine reordering_ele_size(SGS_param, numele)
!
      use m_work_4_MHD_layering
      use m_filter_elength
      use t_SGS_control_parameter
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      integer(kind = kint), intent(in) :: numele
!
!
      if(SGS_param%iflag_SGS .ne. id_SGS_NL_grad) return
        call allocate_dx_ordering_tmp(numele)
!
        call reordering_scalar_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%moms%f_x2)
        call reordering_scalar_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%moms%f_y2)
        call reordering_scalar_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%moms%f_z2)
        call reordering_scalar_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%moms%f_xy)
        call reordering_scalar_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%moms%f_yz)
        call reordering_scalar_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%moms%f_zx)
!
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff%df_x2)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff%df_y2)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff%df_z2)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff%df_xy)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff%df_yz)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff%df_zx)
!
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff2%df_x2)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff2%df_y2)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff2%df_z2)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff2%df_xy)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff2%df_yz)
        call reordering_vector_by_layer(numele, old2newele_layer(1),    &
     &      FEM1_elen%elen_ele%diff2%df_zx)
!
        call deallocate_dx_ordering_tmp
!
!
!      if(SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
!        call reordering_layer_info(numele, old2newele_layer(1),        &
!     &      layer_tbl1%e_grp%num_item, item_layer(1) )
!      end if
!
      end subroutine reordering_ele_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine reordering_scalar_by_layer(numele, old2newele, elen)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: old2newele(numele)
!
      real(kind = kreal), intent(inout) :: elen(numele)
!
      integer(kind = kint) :: iele, inum
!
!$omp parallel do
      do iele = 1, numele
        dx_ordering_tmp(iele) = elen(iele)
      end do
!$omp end parallel do
!
!!$omp parallel do private (inum,iele)
      do inum = 1, numele
        iele = old2newele(inum)
        elen(iele) = dx_ordering_tmp(inum)
      end do
!!$omp end parallel do
!
      end subroutine reordering_scalar_by_layer
!
!------------------------------------------------------------------
!
      subroutine reordering_vector_by_layer(numele, old2newele, elen)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: old2newele(numele)
!
      real(kind = kreal), intent(inout) :: elen(numele,3)
!
      integer(kind = kint) :: nd, iele, inum
!
      do nd = 1, 3
!
!$omp parallel do
        do iele = 1, numele
          dx_ordering_tmp(iele) = elen(iele,nd)
        end do
!$omp end parallel do
!
!!$omp parallel do private (inum,iele)
        do inum = 1, numele
          iele = old2newele(inum)
          elen(iele,nd) = dx_ordering_tmp(inum)
        end do
!!$omp end parallel do
!
      end do
!
      end subroutine reordering_vector_by_layer
!
!------------------------------------------------------------------
!
      subroutine reordering_layer_info(numele, old2newele,              &
     &          n_item_layer_d, item_layer)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: old2newele(numele)
      integer (kind = kint), intent(in) :: n_item_layer_d
!
      integer (kind = kint), intent(inout) :: item_layer(n_item_layer_d)
!
      integer (kind = kint) :: iele, iele0, inum
!
!$omp parallel do private (iele,iele0)
      do inum = 1, n_item_layer_d
        iele0 = item_layer(inum)
        iele = old2newele(iele0)
        item_layer(inum) = iele
      end do
!$omp end parallel do
!
      end subroutine reordering_layer_info
!
!------------------------------------------------------------------
!
      end module reordering_element_size
