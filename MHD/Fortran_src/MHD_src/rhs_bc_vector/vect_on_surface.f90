!vect_on_surface.f90
!     module vect_on_surface
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine vector_on_surface(ngrp_sf, id_grp_sf, n_int, i_vect)
!      subroutine diff_vector_on_surface(ngrp_sf, id_grp_sf,            &
!     &          n_int, i_vect)
!
      module vect_on_surface
!
      use m_precision
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_surface_group
      use m_surface_group_geometry
      use m_int_surface_data
      use m_fem_gauss_int_coefs
      use m_jacobians_2d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine vector_on_surface(ngrp_sf, id_grp_sf, n_int, i_vect)
!
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: ngrp_sf
      integer (kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint), intent(in) :: i_vect
!
      integer (kind = kint) :: i, igrp, iproc, id_sf, nsf
      integer (kind = kint) :: ist, ied, inum, iele, isf, inod
      integer (kind = kint) :: k1, kk1, ii, ix
!
!
      vect_sf = 0.0d0
!
      do i = 1, ngrp_sf
       igrp = id_grp_sf(i)
!
       nsf = surf_istack(igrp) - surf_istack(igrp-1)
       if (nsf.gt.0) then
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,k1,kk1,ii,ix,inod)
        do iproc = 1, np_smp
         id_sf = np_smp*(igrp-1) + iproc
         ist = isurf_grp_smp_stack(id_sf-1)+1
         ied = isurf_grp_smp_stack(id_sf)
!
         do k1 = 1, nnod_4_surf
          do ii= 1, n_int * n_int
           ix = int_start2(n_int) + ii
!
!cdir nodep
!VOPTION INDEP, VEC
           do inum = ist, ied
!
            iele = surf_item(1,inum)
            isf = surf_item(2,inum)
            kk1 =   node_on_sf(k1,isf)
            inod =   ie(iele,kk1)
!
            vect_sf(inum,1) = vect_sf(inum,1)                           &
     &                       + aw_sf(inum,k1) * d_nod(inod,i_vect  )    &
     &                        * xjq_sf(inum,ix) * owe2d(ix)             &
     &                        * sf_grp_v1%a_area_sf_grp(inum)
            vect_sf(inum,2) = vect_sf(inum,2)                           &
     &                       + aw_sf(inum,k1) * d_nod(inod,i_vect+1)    &
     &                        * xjq_sf(inum,ix) * owe2d(ix)             &
     &                        * sf_grp_v1%a_area_sf_grp(inum)
            vect_sf(inum,3) = vect_sf(inum,3)                           &
     &                       + aw_sf(inum,k1) * d_nod(inod,i_vect+2)    &
     &                        * xjq_sf(inum,ix) * owe2d(ix)             &
     &                        * sf_grp_v1%a_area_sf_grp(inum)
!
           end do
          end do
         end do
!
        end do
!$omp end parallel do
!
       end if
      end do
!
      end subroutine vector_on_surface
!
!-----------------------------------------------------------------------
!
      subroutine diff_vector_on_surface(ngrp_sf, id_grp_sf,             &
     &          n_int, i_vect)
!
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: ngrp_sf
      integer (kind = kint), intent(in) :: id_grp_sf(ngrp_sf)
      integer (kind = kint), intent(in) :: n_int
      integer (kind = kint), intent(in) :: i_vect
!
      integer (kind = kint) :: i, igrp, iproc, id_sf, nsf
      integer (kind = kint) :: ist, ied, inum, iele, isf, inod, inod_n
      integer (kind = kint) :: k1, kk1, kk1_n, ii, ix
!
!
      vect_sf = 0.0d0
!
      do i = 1, ngrp_sf
       igrp = id_grp_sf(i)
!
       nsf = surf_istack(igrp) - surf_istack(igrp-1)
       if (nsf.gt.0) then
!
!$omp parallel do &
!$omp& private(id_sf,ist,ied,inum,iele,isf,k1,kk1,kk1_n,ii,ix, &
!$omp& inod,inod_n) 
        do iproc = 1, np_smp
         id_sf = np_smp*(igrp-1) + iproc
         ist = isurf_grp_smp_stack(id_sf-1)+1
         ied = isurf_grp_smp_stack(id_sf)
!
         do k1 = 1, nnod_4_surf
          do ii= 1, n_int * n_int
           ix = int_start2(n_int) + ii
!
!cdir nodep
!VOPTION INDEP, VEC
           do inum = ist, ied
!
            iele = surf_item(1,inum)
            isf =  surf_item(2,inum)
            kk1 =   node_on_sf(k1,isf)
            kk1_n = node_on_sf(k1,isf)
            inod =   ie(iele,kk1)
            inod_n = ie(iele,kk1)
!
            vect_sf(inum,1) = vect_sf(inum,1) + aw_sf(inum,k1)          &
     &         * ( d_nod(inod,i_vect  ) - d_nod(inod_n,i_vect  ) )      &
     &         * xjq_sf(inum,ix) * owe2d(ix)                            &
     &         * sf_grp_v1%a_area_sf_grp(inum)
            vect_sf(inum,2) = vect_sf(inum,2) + aw_sf(inum,k1)          &
     &         * ( d_nod(inod,i_vect+1) - d_nod(inod_n,i_vect+1) )      &
     &         * xjq_sf(inum,ix) * owe2d(ix)                            &
     &         * sf_grp_v1%a_area_sf_grp(inum)
            vect_sf(inum,3) = vect_sf(inum,3) + aw_sf(inum,k1)          &
     &         * ( d_nod(inod,i_vect+2) - d_nod(inod_n,i_vect+2) )      &
     &         * xjq_sf(inum,ix) * owe2d(ix)                            &
     &         * sf_grp_v1%a_area_sf_grp(inum)
!
           end do
          end do
         end do
!
        end do
!$omp end parallel do
!
       end if
      end do
!
      end subroutine diff_vector_on_surface
!
!-----------------------------------------------------------------------
!
      end module vect_on_surface
