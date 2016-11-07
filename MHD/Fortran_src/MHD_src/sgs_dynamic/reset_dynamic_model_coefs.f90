!
!      module reset_dynamic_model_coefs
!
!     Written by H. Matsui
!     Modified by H. Matsui on July, 2007
!
!!      subroutine reset_vector_sgs_model_coefs                         &
!!     &         (ele, layer_tbl, icomp_sgs, sgs_coefs)
!!      subroutine reset_tensor_sgs_model_coefs                         &
!!     &         (ele, layer_tbl, icomp_sgs, sgs_coefs)
!!
!!      subroutine reset_diff_model_coefs                               &
!!     &         (numele, iele_smp_stack, ncomp_diff, iak_diff, ak_diff)
!!
!!      subroutine reset_vector_sgs_nod_m_coefs(numnod, inod_smp_stack, &
!!     &          ntot_comp_nod, icomp_sgs, ak_sgs_nod)
!!      subroutine reset_tensor_sgs_nod_m_coefs(numnod, inod_smp_stack, &
!!     &          ntot_comp_nod, icomp_sgs, ak_sgs_nod)
!
      module reset_dynamic_model_coefs
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
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
      subroutine reset_vector_sgs_model_coefs                           &
     &         (ele, layer_tbl, icomp_sgs, sgs_coefs)
!
      use t_geometry_data
      use t_layering_ele_list
      use t_material_property
!
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: icomp_sgs
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
      if(layer_tbl%minlayer_4_smp                                       &
     &     .gt. layer_tbl%min_item_layer_d_smp) then
        call reset_sgs_v_model_coefs_elesmp                             &
     &     (icomp_sgs, ele%numele, ele%istack_ele_smp,                  &
     &      layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,          &
     &      layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,   &
     &      sgs_coefs%ntot_comp, sgs_coefs%ak)
      else
        call reset_sgs_v_model_coefs_grpsmp(icomp_sgs, ele%numele,      &
     &     layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,           &
     &     layer_tbl%e_grp%istack_grp,                                  &
     &     layer_tbl%istack_item_layer_d_smp, layer_tbl%e_grp%item_grp, &
     &     sgs_coefs%ntot_comp, sgs_coefs%ak)
      end if
!
      end subroutine reset_vector_sgs_model_coefs
!
!-----------------------------------------------------------------------
!
      subroutine reset_tensor_sgs_model_coefs                           &
     &         (ele, layer_tbl, icomp_sgs, sgs_coefs)
!
      use t_geometry_data
      use t_layering_ele_list
      use t_material_property
!
      type(element_data), intent(in) :: ele
      type(layering_tbl), intent(in) :: layer_tbl
      integer (kind = kint), intent(in) :: icomp_sgs
!
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!
!
      if(layer_tbl%minlayer_4_smp                                       &
     &     .gt. layer_tbl%min_item_layer_d_smp) then
        call reset_sgs_t_model_coefs_elesmp                             &
     &     (icomp_sgs, ele%numele, ele%istack_ele_smp,                  &
     &      layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,          &
     &      layer_tbl%e_grp%istack_grp_smp, layer_tbl%e_grp%item_grp,   &
     &      sgs_coefs%ntot_comp, sgs_coefs%ak)
      else
        call reset_sgs_t_model_coefs_grpsmp(icomp_sgs, ele%numele,      &
     &      layer_tbl%e_grp%num_grp, layer_tbl%e_grp%num_item,          &
     &      layer_tbl%e_grp%istack_grp,                                 &
     &      layer_tbl%istack_item_layer_d_smp,                          &
     &      layer_tbl%e_grp%item_grp,                                   &
     &      sgs_coefs%ntot_comp, sgs_coefs%ak)
      end if
!
      end subroutine reset_tensor_sgs_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_v_model_coefs_elesmp                         &
     &         (icomp_sgs, numele, iele_smp_stack, n_layer_d,           &
     &          n_item_layer_d, layer_stack_smp, item_layer,            &
     &          ntot_comp_ele, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp_ele, icomp_sgs
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
!
      integer (kind = kint) :: iele0, iele, iproc, ist, ied, is, inum
!
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_sgs(iele, icomp_sgs  ) = zero
          ak_sgs(iele, icomp_sgs+1) = zero
          ak_sgs(iele, icomp_sgs+2) = zero
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
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
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
     &         (icomp_sgs, numele, iele_smp_stack, n_layer_d,           &
     &          n_item_layer_d, layer_stack_smp, item_layer,            &
     &          ntot_comp_ele, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp_ele, icomp_sgs
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in)                                 &
     &                      :: layer_stack_smp(0:n_layer_d*np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
!
      integer (kind = kint) :: iele0, iele, iproc, ist, ied, is, inum
!
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_sgs(iele, icomp_sgs  ) = zero
          ak_sgs(iele, icomp_sgs+1) = zero
          ak_sgs(iele, icomp_sgs+2) = zero
          ak_sgs(iele, icomp_sgs+3) = zero
          ak_sgs(iele, icomp_sgs+4) = zero
          ak_sgs(iele, icomp_sgs+5) = zero
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
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
            ak_sgs(iele, icomp_sgs+3) = one
            ak_sgs(iele, icomp_sgs+4) = one
            ak_sgs(iele, icomp_sgs+5) = one
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
      subroutine reset_sgs_v_model_coefs_grpsmp(icomp_sgs, numele,      &
     &          n_layer_d, n_item_layer_d, layer_stack,                 &
     &          istack_item_layer_d_smp, item_layer,                    &
     &          ntot_comp_ele, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ntot_comp_ele, icomp_sgs
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in)                                 &
     &               :: istack_item_layer_d_smp(0:np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
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
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_sgs_v_model_coefs_grpsmp
!
!-----------------------------------------------------------------------
!
      subroutine reset_sgs_t_model_coefs_grpsmp(icomp_sgs, numele,      &
     &          n_layer_d, n_item_layer_d, layer_stack,                 &
     &          istack_item_layer_d_smp, item_layer,                    &
     &          ntot_comp_ele, ak_sgs)
!
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ntot_comp_ele, icomp_sgs
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in)                                 &
     &               :: istack_item_layer_d_smp(0:np_smp)
      integer (kind = kint), intent(in) :: item_layer(n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: ak_sgs(numele,ntot_comp_ele)
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
            ak_sgs(iele, icomp_sgs  ) = one
            ak_sgs(iele, icomp_sgs+1) = one
            ak_sgs(iele, icomp_sgs+2) = one
            ak_sgs(iele, icomp_sgs+3) = one
            ak_sgs(iele, icomp_sgs+4) = one
            ak_sgs(iele, icomp_sgs+5) = one
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
      subroutine reset_diff_model_coefs                                 &
     &         (numele, iele_smp_stack, ncomp_diff, iak_diff, ak_diff)
!
      integer (kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numele, ncomp_diff, iak_diff
      real(kind=kreal), intent(inout) :: ak_diff(numele,ncomp_diff)
!
      integer (kind = kint) :: iele, iproc, ist, ied
!
!$omp parallel do private(iele, ist, ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          ak_diff(iele, iak_diff) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_diff_model_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_vector_sgs_nod_m_coefs(numnod, inod_smp_stack,   &
     &          ntot_comp_nod, icomp_sgs, ak_sgs_nod)
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp_nod, icomp_sgs
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      integer (kind = kint) :: inod, iproc, ist, ied
!
!$omp parallel do private(inod, ist, ied) 
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied = inod_smp_stack(iproc)
        do inod = ist, ied
          ak_sgs_nod(inod, icomp_sgs  ) = one
          ak_sgs_nod(inod, icomp_sgs+1) = one
          ak_sgs_nod(inod, icomp_sgs+2) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_vector_sgs_nod_m_coefs
!
!-----------------------------------------------------------------------
!
      subroutine reset_tensor_sgs_nod_m_coefs(numnod, inod_smp_stack,   &
     &          ntot_comp_nod, icomp_sgs, ak_sgs_nod)
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: ntot_comp_nod, icomp_sgs
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ak_sgs_nod(numnod,ntot_comp_nod)
!
      integer (kind = kint) :: inod, iproc, ist, ied
!
!$omp parallel do private(inod, ist, ied) 
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1) + 1
        ied = inod_smp_stack(iproc)
        do inod = ist, ied
          ak_sgs_nod(inod, icomp_sgs  ) = one
          ak_sgs_nod(inod, icomp_sgs+1) = one
          ak_sgs_nod(inod, icomp_sgs+2) = one
          ak_sgs_nod(inod, icomp_sgs+3) = one
          ak_sgs_nod(inod, icomp_sgs+4) = one
          ak_sgs_nod(inod, icomp_sgs+5) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine reset_tensor_sgs_nod_m_coefs
!
!-----------------------------------------------------------------------
!
      end module reset_dynamic_model_coefs
