!cal_filter_moms_by_element.f90
!     module cal_filter_moms_by_element
!
      module cal_filter_moms_by_element
!
!     Written by H. Matsui on Apr., 2008
!
      use m_precision
!
      use m_reference_moments
      use m_filter_elength
      use m_finite_element_matrix
      use m_phys_constants
!
      implicit none
!
!      subroutine cal_filter_moments_on_ele
!      subroutine cal_filter_moments_on_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_ele
!
      use m_machine_parameter
      use m_filter_dxdxi
      use cal_1d_moments_4_fliter
      use set_filter_moments_3d
!
      integer(kind = kint) :: iele
!
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_moments_order'
      call s_set_moments_order
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_table_4_moments'
      call s_set_table_4_moments
      if (iflag_debug.eq.1)  write(*,*) 's_cal_1d_moments_4_filter'
      call s_cal_1d_moments_4_filter
!
!      if (iflag_debug.eq.1)                                            &
!     & write(*,*) 's_set_seeds_moments', nele_filter_mom, num_order_3d
      do iele = 1, nele_filter_mom
        call s_set_seeds_moments                                        &
     &      (filter_dxi1%dxi_ele%dx%df_dxi(iele),                       &
     &       filter_dxi1%dxi_ele%dx%df_dei(iele),                       &
     &       filter_dxi1%dxi_ele%dx%df_dzi(iele),                       &
     &       filter_dxi1%dxi_ele%dy%df_dxi(iele),                       &
     &       filter_dxi1%dxi_ele%dy%df_dei(iele),                       &
     &       filter_dxi1%dxi_ele%dy%df_dzi(iele),                       &
     &       filter_dxi1%dxi_ele%dz%df_dxi(iele),                       &
     &       filter_dxi1%dxi_ele%dz%df_dei(iele),                       &
     &       filter_dxi1%dxi_ele%dz%df_dzi(iele))
!
        seed_moments_ele(iele,1:num_order_3d)                           &
     &      = seed_moments(1:num_order_3d)
!
      end do
!
      end subroutine cal_filter_moments_on_ele
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_moments_on_node
!
      use m_ctl_params_4_gen_filter
      use filter_moments_send_recv
      use int_vol_elesize_on_node
      use nodal_vector_send_recv
!
      integer(kind = kint) :: inod, n, im_x, im_y, im_z
!
!
      do n = 1, num_order_3d
!
        im_x = iorder_mom_3d(n,1)
        im_y = iorder_mom_3d(n,2)
        im_z = iorder_mom_3d(n,3)
!
        if ( mod((im_x+im_y+im_z),2) .eq. 1) then
          seed_moments_nod(1:nnod_filter_mom,n) = 0.0d0
        else if(im_x.eq.0 .and. im_y.eq.0 .and. im_z.eq.0) then
          seed_moments_nod(1:nnod_filter_mom,n) = 1.0d0
!
        else if(im_x.eq.2 .and. im_y.eq.0 .and. im_z.eq.0) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dx2_nod(inod)               &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.0 .and. im_y.eq.2 .and. im_z.eq.0) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dy2_nod(inod)               &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.0 .and. im_y.eq.0 .and. im_z.eq.2) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dz2_nod(inod)               &
     &                              * ref_moments_1d(2)
          end do
!
        else if(im_x.eq.1 .and. im_y.eq.1 .and. im_z.eq.0) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dxdy_nod(inod)              &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.0 .and. im_y.eq.1 .and. im_z.eq.1) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dydz_nod(inod)              &
     &                              * ref_moments_1d(2)
          end do
        else if(im_x.eq.1 .and. im_y.eq.0 .and. im_z.eq.1) then
          do inod = 1, nnod_filter_mom
            seed_moments_nod(inod,n) = elen_dzdx_nod(inod)              &
     &                              * ref_moments_1d(2)
          end do
!
        else
!
          call int_dx_ele2_node(itype_mass_matrix,                      &
     &        seed_moments_ele(1,n), seed_moments_nod(1,n))
          call nod_scalar_send_recv(seed_moments_nod(1,n))
!
        end if
!
      end do
!
      end subroutine cal_filter_moments_on_node
!
!-----------------------------------------------------------------------
!
      end module cal_filter_moms_by_element
