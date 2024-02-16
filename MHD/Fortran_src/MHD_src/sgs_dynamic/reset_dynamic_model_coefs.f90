!
!      module reset_dynamic_model_coefs
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!
!!      subroutine clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!!      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!
!!      subroutine reset_vector_sgs_model_coefs(ele, layer_tbl, Csim)
!!      subroutine reset_tensor_sgs_model_coefs(ele, layer_tbl, Csim)
!!
!!      subroutine reset_diff_model_coefs(numele, iele_smp_stack, Cdiff)
!!        integer (kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
!!        integer (kind = kint), intent(in) :: numele
!!        type(SGS_model_coefficient), intent(inout) :: Cdiff
!!
!!      subroutine reset_vector_sgs_nod_m_coefs(numnod, inod_smp_stack, &
!!     &                                        ak_sgs_nod)
!!      subroutine reset_tensor_sgs_nod_m_coefs(numnod, inod_smp_stack, &
!!     &                                        ak_sgs_nod)
!
      module reset_dynamic_model_coefs
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
      private :: reset_sgs_v_model_coefs_elesmp
      private :: reset_sgs_t_model_coefs_elesmp
      private :: reset_sgs_v_model_coefs_grpsmp
      private :: reset_sgs_t_model_coefs_grpsmp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
      use m_phys_constants
      use t_SGS_model_coef_labels
      use t_phys_data
      use copy_nodal_fields
!
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(inout) :: nod_fld
!
!
      call clear_field_data(nod_fld, n_sym_tensor, iphys_SGS_wk%i_simi)
      call clear_field_data(nod_fld, n_sym_tensor, iphys_SGS_wk%i_nlg)
      call clear_field_data                                             &
     &   (nod_fld, n_sym_tensor, iphys_SGS_wk%i_wd_nlg)
!
      end subroutine clear_work_4_dynamic_model
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine reset_vector_sgs_model_coefs(ele, layer_tbl, Csim)
!
      use t_geometry_data
      use t_layering_ele_list
      use t_SGS_model_coefs
!
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      if(layer_tbl%minlayer_4_smp                                       &
     &     .gt. layer_tbl%min_item_layer_d_smp) then
        call reset_sgs_v_model_coefs_elesmp                             &
     &     (ele%numele, ele%istack_ele_smp,                             &
     &      layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,          &
     &      layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,   &
     &      Csim%coef(1,1))
      else
        call reset_sgs_v_model_coefs_grpsmp(ele%numele,                 &
     &     layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,           &
     &     layer_tbl%e_grp%istack_grp,                                  &
     &     layer_tbl%istack_item_layer_d_smp, layer_tbl%e_grp%item_grp, &
     &      Csim%coef(1,1))
      end if
!
      end subroutine reset_vector_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine reset_tensor_sgs_model_coefs(ele, layer_tbl, Csim)
!
      use t_geometry_data
      use t_layering_ele_list
      use t_SGS_model_coefs
!
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(SGS_model_coefficient), intent(inout) :: Csim
!
!
      if(layer_tbl%minlayer_4_smp                                       &
     &     .gt. layer_tbl%min_item_layer_d_smp) then
        call reset_sgs_t_model_coefs_elesmp                             &
     &     (ele%numele, ele%istack_ele_smp,                             &
     &      layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,          &
     &      layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,   &
     &      Csim%coef(1,1))
      else
        call reset_sgs_t_model_coefs_grpsmp(ele%numele,                 &
     &      layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,          &
     &      layer_tbl%e_grp%istack_grp,                                 &
     &      layer_tbl%istack_item_layer_d_smp,                          &
     &      layer_tbl%e_grp%item_grp, Csim%coef(1,1))
      end if
!
      end subroutine reset_tensor_sgs_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_v_model_coefs_elesmp                         &
     &         (numele, iele_smp_stack, n_layer_d,                      &
     &          n_item_layer_d, layer_stack_smp, item_layer, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,n_vector)
!
      integer (kind = kint) :: iele0, iele, iproc, ist, ied, is, inum
!
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_sgs(iele, 1) = zero
          ak_sgs(iele, 2) = zero
          ak_sgs(iele, 3) = zero
        end do
      end do
!$omp end parallel do
!
!$omp parallel
      do inum = 1, n_layer_d
!$omp do private(iele0,iele,is,ist,ied) 
        do iproc = 1, np_smp
          is = (inum-1)*np_smp + iproc
          ist = layer_stack_smp(is-1) + 1
          ied = layer_stack_smp(is  )
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, 1) = one
            ak_sgs(iele, 2) = one
            ak_sgs(iele, 3) = one
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine reset_sgs_v_model_coefs_elesmp
!
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_t_model_coefs_elesmp                         &
     &         (numele, iele_smp_stack, n_layer_d,                      &
     &          n_item_layer_d, layer_stack_smp, item_layer, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,n_sym_tensor)
!
      integer (kind = kint) :: iele0, iele, iproc, ist, ied, is, inum
!
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_sgs(iele, 1) = zero
          ak_sgs(iele, 2) = zero
          ak_sgs(iele, 3) = zero
          ak_sgs(iele, 4) = zero
          ak_sgs(iele, 5) = zero
          ak_sgs(iele, 6) = zero
        end do
      end do
!$omp end parallel do
!
!$omp parallel
      do inum = 1, n_layer_d
!$omp do private(iele0,iele,is,ist,ied) 
        do iproc = 1, np_smp
          is = (inum-1)*np_smp + iproc
          ist = layer_stack_smp(is-1) + 1
          ied = layer_stack_smp(is  )
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, 1) = one
            ak_sgs(iele, 2) = one
            ak_sgs(iele, 3) = one
            ak_sgs(iele, 4) = one
            ak_sgs(iele, 5) = one
            ak_sgs(iele, 6) = one
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine reset_sgs_t_model_coefs_elesmp
!
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_v_model_coefs_grpsmp                         &
     &         (numele, n_layer_d, n_item_layer_d, layer_stack,         &
     &          istack_item_layer_d_smp, item_layer, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in)                                 &
     &               :: istack_item_layer_d_smp(0:np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,n_vector)
!
      integer (kind = kint) :: iele0, iele, iproc, inum
      integer (kind = kint) :: ist, ied, ist_num, ied_num
!
!
!$omp parallel do private(iele0,iele,ist,ied,ist_num,ied_num,inum)
      do iproc = 1, np_smp
        ist_num = istack_item_layer_d_smp(iproc-1) + 1
        ied_num = istack_item_layer_d_smp(iproc  )
        do inum = ist_num, ied_num
          ist = layer_stack(inum-1) + 1
          ied = layer_stack(inum)
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, 1) = one
            ak_sgs(iele, 2) = one
            ak_sgs(iele, 3) = one
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_sgs_v_model_coefs_grpsmp
!
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_t_model_coefs_grpsmp                         &
     &         (numele, n_layer_d, n_item_layer_d, layer_stack,         &
     &          istack_item_layer_d_smp, item_layer, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in)                                 &
     &               :: istack_item_layer_d_smp(0:np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,n_sym_tensor)
!
      integer (kind = kint) :: iele0, iele, iproc, inum
      integer (kind = kint) :: ist, ied, ist_num, ied_num
!
!
!$omp parallel do private(iele0,iele,ist,ied,ist_num,ied_num,inum)
      do iproc = 1, np_smp
        ist_num = istack_item_layer_d_smp(iproc-1) + 1
        ied_num = istack_item_layer_d_smp(iproc  )
        do inum = ist_num, ied_num
          ist = layer_stack(inum-1) + 1
          ied = layer_stack(inum)
!$cdir nodep
          do iele0 = ist, ied
            iele = item_layer(iele0)
            ak_sgs(iele, 1) = one
            ak_sgs(iele, 2) = one
            ak_sgs(iele, 3) = one
            ak_sgs(iele, 4) = one
            ak_sgs(iele, 5) = one
            ak_sgs(iele, 6) = one
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_sgs_t_model_coefs_grpsmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_diff_model_coefs(numele, iele_smp_stack, Cdiff)
!
      use t_SGS_model_coefs
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numele
      type(SGS_model_coefficient), intent(inout) :: Cdiff
!
      integer (kind = kint) :: ist, ied
!
      ist = iele_smp_stack(0) + 1
      ied = iele_smp_stack(np_smp)
!$omp parallel
      call constant_scalar_smp(one, numele, ist, ied, Cdiff%coef(1,1))
!$omp end parallel
!
      end subroutine reset_diff_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_vector_sgs_nod_m_coefs(numnod, inod_smp_stack,   &
     &                                        ak_sgs_nod)
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(inout) :: ak_sgs_nod(numnod,n_vector)
!
      integer (kind = kint) :: ist, ied
!
!
      ist = inod_smp_stack(0) + 1
      ied = inod_smp_stack(np_smp)
!$omp parallel 
      call constant_vector_smp(one, numnod, ist, ied, ak_sgs_nod)
!$omp end parallel
!
      end subroutine reset_vector_sgs_nod_m_coefs
!
!-----------------------------------------------------------------------
!
      subroutine reset_tensor_sgs_nod_m_coefs(numnod, inod_smp_stack,   &
     &                                        ak_sgs_nod)
!
      use delete_field_smp
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ak_sgs_nod(numnod,n_sym_tensor)
!
      integer (kind = kint) :: ist, ied
!
!
      ist = inod_smp_stack(0) + 1
      ied = inod_smp_stack(np_smp)
!$omp parallel 
      call constant_sym_tensor_smp(one, numnod, ist, ied, ak_sgs_nod)
!$omp end parallel
!
      end subroutine reset_tensor_sgs_nod_m_coefs
!
!-----------------------------------------------------------------------
!
      end module reset_dynamic_model_coefs
