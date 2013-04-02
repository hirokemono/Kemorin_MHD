!
!      module int_surf_rot_sgs
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine int_surf_rotation_sgs(n_int, nmax_grp_sf, ngrp_sf,    &
!     &         id_grp_sf, i_filter, iak_diff, i_vect)
!      subroutine int_surf_rot_commute_sgs(n_int, nmax_grp_sf, ngrp_sf, &
!     &         id_grp_sf, i_filter, i_vect)
!
      module int_surf_rot_sgs
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_surface_group
      use m_finite_element_matrix
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_rotation_sgs(n_int, nmax_grp_sf, ngrp_sf,     &
     &         id_grp_sf, i_filter, iak_diff, i_vect)
!
      use m_int_surface_data
      use m_SGS_model_coefs
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_1
      use cal_skv_to_ff_smp_1st
!
      integer(kind = kint), intent(in) :: n_int, nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_vect, iak_diff, i_filter
!
      integer(kind=kint) :: k2, nd, nrot1, nrot2, i, igrp, i_comp
!
!
!  ---------  set number of integral points
!
      if(nmax_grp_sf .eq. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
!
        nrot1 = mod(nd  ,3)+1
        nrot2 = mod(nd+1,3)+1
!
! --------- set vector at each node in an element
!
        i_comp = i_vect + nrot2 - 1
        do i = 1, ngrp_sf(nrot2)
          igrp = id_grp_sf(i,nrot2)
          if((surf_istack(igrp) - surf_istack(igrp-1)) .gt. 0) then
!
            do k2=1, nnod_4_surf
              call dlt_scl_phys_2_each_surface(igrp, k2, i_comp,        &
     &              scalar_sf)
              call fem_sf_skv_sgs_vect_diff_p1(igrp, k2, nd, n_int,     &
     &              i_filter, nrot1, dxe_sf, scalar_sf,                 &
     &              ak_diff(1,iak_diff), one, sk6)
              end do
!
          end if
        end do
!
        i_comp = i_vect + nrot1 - 1
        do i = 1, ngrp_sf(nrot1)
          igrp = id_grp_sf(i,nrot1)
          if((surf_istack(igrp) - surf_istack(igrp-1)) .gt. 0) then
!
            do k2=1, nnod_4_surf
              call dlt_scl_phys_2_each_surf_cst(igrp, k2, i_comp,       &
     &              dminus, scalar_sf)
              call fem_sf_skv_sgs_vect_diff_p1(igrp, k2, nd, n_int,     &
     &              i_filter, nrot2, dxe_sf, scalar_sf,                 &
     &              ak_diff(1,iak_diff), one, sk6)
            end do
!
          end if
        end do
!
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_surf_rotation_sgs
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_rot_commute_sgs(n_int, nmax_grp_sf, ngrp_sf,  &
     &         id_grp_sf, i_filter, i_vect)
!
      use m_int_surface_data
      use delta_phys_2_each_surface
      use fem_surf_skv_sgs_commute_1
      use cal_skv_to_ff_smp_1st
!
      integer(kind = kint), intent(in) :: n_int, nmax_grp_sf
      integer(kind = kint), intent(in) :: ngrp_sf(3)
      integer(kind = kint), intent(in) :: id_grp_sf(nmax_grp_sf,3)
      integer(kind = kint), intent(in) :: i_vect, i_filter
!
      integer(kind=kint) :: k2, nd, nrot1, nrot2, i, igrp, i_comp
!
!
!  ---------  set number of integral points
!
      if(nmax_grp_sf .eq. 0) return
      call reset_sk6(n_vector)
!
      do nd = 1, n_vector
        nrot1 = mod(nd  ,3)+1
        nrot2 = mod(nd+1,3)+1
!
        i_comp = i_vect + nrot2 - 1
        do i = 1, ngrp_sf(nrot2)
          igrp = id_grp_sf(i,nrot2)
          if((surf_istack(igrp) - surf_istack(igrp-1)) .gt. 0) then
!
            do k2=1, nnod_4_surf
             call dlt_scl_phys_2_each_surface(igrp, k2, i_comp,         &
     &             scalar_sf)
             call fem_sf_skv_sgs_commute_err_p1(igrp, k2, nd, n_int,    &
     &               i_filter, nrot1, dxe_sf, scalar_sf, sk6)
           end do
!
         end if
       end do
!
       i_comp = i_vect + nrot1 - 1
       do i = 1, ngrp_sf(nrot1)
         igrp = id_grp_sf(i,nrot1)
         if((surf_istack(igrp) - surf_istack(igrp-1)) .gt. 0) then
!
            do k2=1, nnod_4_surf
              call dlt_scl_phys_2_each_surf_cst(igrp, k2,               &
     &            i_comp, dminus, scalar_sf)
              call fem_sf_skv_sgs_commute_err_p1(igrp, k2, nd, n_int,   &
     &                i_filter, nrot2, dxe_sf, scalar_sf, sk6)
            end do
!
          end if
        end do
!
      end do
!
      call add3_skv_to_ff_v_smp_1st(ff_nl_smp, sk6)
!
      end subroutine int_surf_rot_commute_sgs
!
!-----------------------------------------------------------------------
!
      end module int_surf_rot_sgs
