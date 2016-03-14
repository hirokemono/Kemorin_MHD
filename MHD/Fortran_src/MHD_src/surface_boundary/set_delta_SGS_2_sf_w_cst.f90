!set_delta_SGS_2_sf_w_cst.f90
!      module set_delta_SGS_2_sf_w_cst
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine delta_flux_t_2_each_sf_w_cst                          &
!     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,   &
!     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,      &
!     &          num_surf_smp, isurf_grp_smp_stack, igrp,               &
!     &          k2, nd, i_vect, i_field, i_flux, ntot_comp, d_nod,     &
!     &          coef, vector_sf)
!      subroutine delta_SGS_induct_t_2_sf_w_cst                         &
!     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,   &
!     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,      &
!     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2, nd,       &
!     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
!
      module set_delta_SGS_2_sf_w_cst
!
      use m_precision
      use m_geometry_constants
      use m_phys_constants
!
      implicit none
!
      private :: d_SGS_induct_x_2_surf_w_cst
      private :: d_SGS_induct_y_2_surf_w_cst
      private :: d_SGS_induct_z_2_surf_w_cst
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine delta_flux_t_2_each_sf_w_cst                           &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2, nd,        &
     &          i_vect, i_field, i_flux, ntot_comp, d_nod,              &
     &          coef, vector_sf)
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
      integer (kind = kint), intent(in) :: i_vect, i_field, i_flux
      integer (kind = kint), intent(in) :: igrp, k2, nd
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(num_surf_bc,3)
!
      integer (kind = kint) :: iproc, id_sf, ist, ied, isf
      integer (kind = kint) :: inum, iele, inod, inod_n
      integer (kind = kint) :: kk2, kk2_n, i_scalar
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = i_flux + l_sim_t(1,nd)
      n2 = i_flux + l_sim_t(2,nd)
      n3 = i_flux + l_sim_t(3,nd)
      i_scalar = i_field + nd - 1
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
          vector_sf(inum,1) = coef                                      &
     &            * ( d_nod(inod,  i_vect  ) * d_nod(inod,  i_scalar)   &
     &              - d_nod(inod_n,i_vect  ) * d_nod(inod_n,i_scalar)   &
     &              + d_nod(inod,  n1      ) - d_nod(inod_n,n1      ) )
          vector_sf(inum,2) = coef                                      &
     &            * ( d_nod(inod,  i_vect+1) * d_nod(inod,  i_scalar)   &
     &              - d_nod(inod_n,i_vect+1) * d_nod(inod_n,i_scalar)   &
     &              + d_nod(inod,  n2      ) - d_nod(inod_n,n2      ) )
          vector_sf(inum,3) = coef                                      &
     &            * ( d_nod(inod,  i_vect+2) * d_nod(inod,  i_scalar)   &
     &              - d_nod(inod_n,i_vect+2) * d_nod(inod_n,i_scalar)   &
     &              + d_nod(inod,  n3      ) - d_nod(inod_n,n3      ) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine delta_flux_t_2_each_sf_w_cst
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine delta_SGS_induct_t_2_sf_w_cst                          &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2, nd,        &
     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
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
     &                      :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
      integer (kind = kint), intent(in) :: igrp, k2, nd
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(num_surf_bc,3)
!
!
      if(nd .eq. 1) then
        call d_SGS_induct_x_2_surf_w_cst                                &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2,            &
     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
      else if(nd .eq. 2) then
        call d_SGS_induct_y_2_surf_w_cst                                &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2,            &
     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
      else if(nd .eq. 3) then
        call d_SGS_induct_z_2_surf_w_cst                                &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2,            &
     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
      end if
!
      end subroutine delta_SGS_induct_t_2_sf_w_cst
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_induct_x_2_surf_w_cst                            &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2,            &
     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
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
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
      integer (kind = kint), intent(in) :: igrp, k2
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(num_surf_bc,3)
!
      integer (kind = kint) :: iproc, id_sf, ist, ied, isf
      integer (kind = kint) :: inum, iele, inod, inod_n
      integer (kind = kint) :: kk2, kk2_n
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
          vector_sf(inum,1) = 0.0d0
          vector_sf(inum,2) = coef                                      &
     &          * ( d_nod(inod,  i_b+1   ) * d_nod(inod,  i_v     )     &
     &            - d_nod(inod_n,i_b+1   ) * d_nod(inod_n,i_v     )     &
     &            - d_nod(inod,  i_v+1   ) * d_nod(inod,  i_b     )     &
     &            + d_nod(inod_n,i_v+1   ) * d_nod(inod_n,i_b     )     &
     &            + d_nod(inod,  i_flux  ) - d_nod(inod_n,i_flux  ) )
          vector_sf(inum,3) = coef                                      &
     &          * ( d_nod(inod,  i_b+2   ) * d_nod(inod,  i_v     )     &
     &            - d_nod(inod_n,i_b+2   ) * d_nod(inod_n,i_v     )     &
     &            - d_nod(inod,  i_v+2   ) * d_nod(inod,  i_b     )     &
     &            + d_nod(inod_n,i_v+2   ) * d_nod(inod_n,i_b     )     &
     &            + d_nod(inod,  i_flux+1) - d_nod(inod_n,i_flux+1) )
        end do
!
      end do
!$omp end parallel do
!
      end subroutine d_SGS_induct_x_2_surf_w_cst
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_induct_y_2_surf_w_cst                            &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2,            &
     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
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
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
      integer (kind = kint), intent(in) :: igrp, k2
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(num_surf_bc,3)
!
      integer (kind = kint) :: iproc, id_sf, ist, ied, isf
      integer (kind = kint) :: inum, iele, inod, inod_n
      integer (kind = kint) :: kk2, kk2_n
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
!ocl vector, novrec
!voption, indep, vec
        do inum = ist, ied
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          kk2 =    node_on_sf(k2,isf)
          kk2_n =  node_on_sf_n(k2,isf)
          inod =   ie(iele,kk2)
          inod_n = ie(iele,kk2_n)
!
          vector_sf(inum,1) = coef                                      &
     &          * ( d_nod(inod,  i_b     ) * d_nod(inod,  i_v+1   )     &
     &            - d_nod(inod_n,i_b     ) * d_nod(inod_n,i_v+1   )     &
     &            - d_nod(inod,  i_v     ) * d_nod(inod,  i_b+1   )     &
     &            + d_nod(inod_n,i_v     ) * d_nod(inod_n,i_b+1   )     &
     &            - d_nod(inod,  i_flux  ) + d_nod(inod_n,i_flux  ) )
          vector_sf(inum,2) = 0.0d0
          vector_sf(inum,3) = coef                                      &
     &          * ( d_nod(inod,  i_b+2   ) * d_nod(inod,  i_v+1   )     &
     &            - d_nod(inod_n,i_b+2   ) * d_nod(inod_n,i_v+1   )     &
     &            - d_nod(inod,  i_v+2   ) * d_nod(inod,  i_b+1   )     &
     &            + d_nod(inod_n,i_v+2   ) * d_nod(inod_n,i_b+1   )     &
     &            + d_nod(inod,  i_flux+2) - d_nod(inod_n,i_flux+2) )
        end do
      end do
!$omp end parallel do
!
      end subroutine d_SGS_induct_y_2_surf_w_cst
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_induct_z_2_surf_w_cst                            &
     &         (np_smp, numnod, numele, nnod_4_ele, ie, nnod_4_surf,    &
     &          node_on_sf, node_on_sf_n, num_surf_bc, surf_item,       &
     &          num_surf_smp, isurf_grp_smp_stack, igrp, k2,            &
     &          i_flux, i_b, i_v, ntot_comp, d_nod, coef, vector_sf)
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
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
      integer (kind = kint), intent(in) :: igrp, k2
!
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(num_surf_bc,3)
!
      integer (kind = kint) :: iproc, id_sf, ist, ied, isf
      integer (kind = kint) :: inum, iele, inod, inod_n
      integer (kind = kint) :: kk2, kk2_n
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
!ocl vector, novrec
!voption, indep, vec
        do inum = ist, ied
          iele = surf_item(1,inum)
          isf =  surf_item(2,inum)
          kk2 =    node_on_sf(k2,isf)
          kk2_n =  node_on_sf_n(k2,isf)
          inod =   ie(iele,kk2)
          inod_n = ie(iele,kk2_n)
!
          vector_sf(inum,1) = coef                                      &
     &           * (d_nod(inod,  i_b     ) * d_nod(inod,  i_v+2   )     &
     &            - d_nod(inod_n,i_b     ) * d_nod(inod_n,i_v+2   )     &
     &            - d_nod(inod,  i_v     ) * d_nod(inod,  i_b+2   )     &
     &            + d_nod(inod_n,i_v     ) * d_nod(inod_n,i_b+2   )     &
     &            - d_nod(inod,  i_flux+1) + d_nod(inod_n,i_flux+1) )
          vector_sf(inum,2) = coef                                      &
     &           * (d_nod(inod,  i_b+1   ) * d_nod(inod,  i_v+2   )     &
     &            - d_nod(inod_n,i_b+1   ) * d_nod(inod_n,i_v+2   )     &
     &            - d_nod(inod,  i_v+1   ) * d_nod(inod,  i_b+2   )     &
     &            + d_nod(inod_n,i_v+1   ) * d_nod(inod_n,i_b+2   )     &
     &            - d_nod(inod,  i_flux+2) + d_nod(inod_n,i_flux+2) )
          vector_sf(inum,3) = 0.0d0
        end do
      end do
!$omp end parallel do
!
      end subroutine d_SGS_induct_z_2_surf_w_cst
!
! ----------------------------------------------------------------------
!
      end module set_delta_SGS_2_sf_w_cst
