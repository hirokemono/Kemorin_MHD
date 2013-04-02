!set_delta_2_each_sf_w_coef.f90
!      module set_delta_2_each_sf_w_coef
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine dlt_scl_phys_2_each_sf_w_coef(np_smp, numnod, numele, &
!     &          nnod_4_ele, ie, nnod_4_surf, node_on_sf, node_on_sf_n, &
!     &          num_surf_bc, surf_item, num_surf_smp,                  &
!     &          isurf_grp_smp_stack, igrp, k2, i_scalar,               &
!     &          ntot_comp, d_nod, ak_e, scalar_sf)
!        Input:  d_nod(1,i_scalar), Output:  scalar_sf
!      subroutine dlt_vect_phys_2_each_sf_w_coef(np_smp, numnod, numele,&
!     &          nnod_4_ele, ie, nnod_4_surf, node_on_sf, node_on_sf_n, &
!     &          num_surf_bc, surf_item, num_surf_smp,                  &
!     &          isurf_grp_smp_stack, igrp, k2, i_vector,               &
!     &          ntot_comp, d_nod, ak_e, vector_sf)
!        Input:  d_nod(1,i_vector), Output:  vector_sf
!      subroutine dlt_tsr_phys_2_each_sf_w_coef(np_smp, numnod, numele, &
!     &          nnod_4_ele, ie, nnod_4_surf, node_on_sf, node_on_sf_n, &
!     &          num_surf_bc, surf_item, num_surf_smp,                  &
!     &          isurf_grp_smp_stack, igrp, k2, i_tensor,               &
!     &          ntot_comp, d_nod, ak_e, tensor_sf)
!        Input:  d_nod(1,i_tensor), Output:  tensor_sf
!
      module set_delta_2_each_sf_w_coef
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine dlt_scl_phys_2_each_sf_w_coef(np_smp, numnod, numele,  &
     &          nnod_4_ele, ie, nnod_4_surf, node_on_sf, node_on_sf_n,  &
     &          num_surf_bc, surf_item, num_surf_smp,                   &
     &          isurf_grp_smp_stack, igrp, k2, i_scalar,                &
     &          ntot_comp, d_nod, ak_e, scalar_sf)
!
      integer(kind = kint), intent(in) :: np_smp, numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_surf
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf_n(nnod_4_surf,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf_bc, num_surf_smp
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: i_scalar
      integer (kind = kint), intent(in) :: igrp, k2
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: scalar_sf(num_surf_bc)
!
      integer (kind = kint) :: iproc, id_sf
      integer (kind = kint) :: ist, ied, inum
      integer (kind = kint) :: iele, isf, kk2, kk2_n, inod, inod_n
!
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,kk2,kk2_n,inod,inod_n)
      do iproc = 1, np_smp
        id_sf = np_smp*(igrp-1) + iproc
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
!cdir nodep
!voption, indep, vec
        do inum = ist, ied
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          kk2 =    node_on_sf(k2,isf)
          kk2_n =  node_on_sf_n(k2,isf)
          inod =   ie(iele,kk2)
          inod_n = ie(iele,kk2_n)
!
          scalar_sf(inum) = ak_e(iele) * (d_nod(inod,i_scalar  )        &
     &                                    - d_nod(inod_n,i_scalar  ))
        end do
      end do
!$omp end parallel do
!
      end subroutine dlt_scl_phys_2_each_sf_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine dlt_vect_phys_2_each_sf_w_coef(np_smp, numnod, numele, &
     &          nnod_4_ele, ie, nnod_4_surf, node_on_sf, node_on_sf_n,  &
     &          num_surf_bc, surf_item, num_surf_smp,                   &
     &          isurf_grp_smp_stack, igrp, k2, i_vector,                &
     &          ntot_comp, d_nod, ak_e, vector_sf)
!
      integer(kind = kint), intent(in) :: np_smp, numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_surf
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf_n(nnod_4_surf,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf_bc, num_surf_smp
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: i_vector
      integer (kind = kint), intent(in) :: igrp, k2
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: vector_sf(num_surf_bc,3)
!
      integer (kind = kint) :: iproc, id_sf
      integer (kind = kint) :: ist, ied, inum
      integer (kind = kint) :: iele, isf, kk2, kk2_n, inod, inod_n
!
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,kk2,kk2_n,inod,inod_n)
      do iproc = 1, np_smp
        id_sf = np_smp*(igrp-1) + iproc
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
!cdir nodep
!voption, indep, vec
        do inum = ist, ied
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          kk2 =    node_on_sf(k2,isf)
          kk2_n =  node_on_sf_n(k2,isf)
          inod =   ie(iele,kk2)
          inod_n = ie(iele,kk2_n)
!
          vector_sf(inum,1) = ak_e(iele) * (d_nod(inod,i_vector  )      &
     &                                      - d_nod(inod_n,i_vector  ))
          vector_sf(inum,2) = ak_e(iele) * (d_nod(inod,i_vector+1)      &
     &                                      - d_nod(inod_n,i_vector+1))
          vector_sf(inum,3) = ak_e(iele) * (d_nod(inod,i_vector+2)      &
     &                                      - d_nod(inod_n,i_vector+2))
        end do
      end do
!$omp end parallel do
!
      end subroutine dlt_vect_phys_2_each_sf_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine dlt_tsr_phys_2_each_sf_w_coef(np_smp, numnod, numele,  &
     &          nnod_4_ele, ie, nnod_4_surf, node_on_sf, node_on_sf_n,  &
     &          num_surf_bc, surf_item, num_surf_smp,                   &
     &          isurf_grp_smp_stack, igrp, k2, i_tensor,                &
     &          ntot_comp, d_nod, ak_e, tensor_sf)
!
      integer(kind = kint), intent(in) :: np_smp, numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_surf
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf_n(nnod_4_surf,nsurf_4_ele)
!
      integer (kind = kint), intent(in) :: num_surf_bc, num_surf_smp
      integer (kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      integer (kind = kint), intent(in)                                 &
     &                       :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: i_tensor
      integer (kind = kint), intent(in) :: igrp, k2
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_e(numele)
!
      real (kind=kreal), intent(inout) :: tensor_sf(num_surf_bc,6)
!
      integer (kind = kint) :: iproc, id_sf
      integer (kind = kint) :: ist, ied, inum
      integer (kind = kint) :: iele, isf, kk2, kk2_n, inod, inod_n
!
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,kk2,kk2_n,inod,inod_n)
      do iproc = 1, np_smp
        id_sf = np_smp*(igrp-1) + iproc
        ist = isurf_grp_smp_stack(id_sf-1)+1
        ied = isurf_grp_smp_stack(id_sf)
!
!cdir nodep
!voption, indep, vec
        do inum = ist, ied
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          kk2 =    node_on_sf(k2,isf)
          kk2_n =  node_on_sf_n(k2,isf)
          inod =   ie(iele,kk2)
          inod_n = ie(iele,kk2_n)
!
          tensor_sf(inum,1) = ak_e(iele) * (d_nod(inod,i_tensor  )      &
     &                                      - d_nod(inod_n,i_tensor  ))
          tensor_sf(inum,2) = ak_e(iele) * (d_nod(inod,i_tensor+1)      &
     &                                      - d_nod(inod_n,i_tensor+1))
          tensor_sf(inum,3) = ak_e(iele) * (d_nod(inod,i_tensor+2)      &
     &                                      - d_nod(inod_n,i_tensor+2))
          tensor_sf(inum,4) = ak_e(iele) * (d_nod(inod,i_tensor+3)      &
     &                                      - d_nod(inod_n,i_tensor+3))
          tensor_sf(inum,5) = ak_e(iele) * (d_nod(inod,i_tensor+4)      &
     &                                      - d_nod(inod_n,i_tensor+4))
          tensor_sf(inum,6) = ak_e(iele) * (d_nod(inod,i_tensor+5)      &
     &                                      - d_nod(inod_n,i_tensor+5))
            end do
          end do
!$omp end parallel do
!
      end subroutine dlt_tsr_phys_2_each_sf_w_coef
!
! ----------------------------------------------------------------------
!
      end module set_delta_2_each_sf_w_coef
