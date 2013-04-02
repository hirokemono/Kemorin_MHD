!copy_filters_4_sorting.f90
!      module copy_filters_4_sorting
!
      module copy_filters_4_sorting
!
!     Written by H. Matsui on May., 2008
!
      use m_precision
!
      use m_nod_filter_comm_table
      use m_new_filter_func_4_sorting
      use m_filter_func_4_sorting
!
      implicit none
!
!      subroutine copy_new_filters_to_original
!      subroutine copy_new_filters_from_original
!
!      subroutine reorder_filter_new_domain
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_new_filters_to_original
!
      integer(kind = kint) :: i
!
!
      inter_nod_3dfilter = intnod_w_fliter2
      ntot_nod_near_w_filter = ntot_nod_near_w_filter2
      ntot_nod_near_f_filter = ntot_nod_near_f_filter2
      call allocate_whole_filter_stack(inter_nod_3dfilter)
      call allocate_fluid_filter_stack(inter_nod_3dfilter)
      call allocate_whole_filter_coefs
      call allocate_fluid_filter_coefs
!
      istack_near_nod_w_filter(0) = istack_near_nod_w_filter2(0)
      istack_near_nod_f_filter(0) = istack_near_nod_f_filter2(0)
      do i = 1, inter_nod_3dfilter
        i_exp_level_w_filter(i) =   i_exp_level_w_filter2(i)
        i_exp_level_f_filter(i) =   i_exp_level_f_filter2(i)
        nnod_near_nod_w_filter(i) =   nnod_near_nod_w_filter2(i)
        nnod_near_nod_f_filter(i) =   nnod_near_nod_f_filter2(i)
        istack_near_nod_w_filter(i) = istack_near_nod_w_filter2(i)
        istack_near_nod_f_filter(i) = istack_near_nod_f_filter2(i)
      end do
!
      do i = 1, ntot_nod_near_w_filter
        inod_near_nod_w_filter(i) = inod_near_nod_w_filter2(i)
        whole_filter_func(i) =      whole_filter_func2(i)
        whole_filter_weight(i) =    whole_filter_weight2(i)
      end do
!
      do i = 1, ntot_nod_near_f_filter
        inod_near_nod_f_filter(i) = inod_near_nod_f_filter2(i)
        fluid_filter_func(i) =      fluid_filter_func2(i)
        fluid_filter_weight(i) =    fluid_filter_weight2(i)
      end do
!
      call deallocate_whole_filter_coefs2
      call deallocate_fluid_filter_coefs2
!
      end subroutine copy_new_filters_to_original
!
! ----------------------------------------------------------------------
!
      subroutine copy_new_filters_from_original
!
      integer(kind = kint) :: i
!
!
      intnod_w_fliter2 = inter_nod_3dfilter
      ntot_nod_near_w_filter2 = ntot_nod_near_w_filter
      ntot_nod_near_f_filter2 = ntot_nod_near_f_filter
      call allocate_whole_filter_stack2
      call allocate_fluid_filter_stack2
      call allocate_whole_filter_coefs2
      call allocate_fluid_filter_coefs2
!
      istack_near_nod_w_filter2(0) = istack_near_nod_w_filter(0)
      istack_near_nod_f_filter2(0) = istack_near_nod_f_filter(0)
      do i = 1, inter_nod_3dfilter
        nnod_near_nod_w_filter2(i) =   nnod_near_nod_w_filter(i)
        nnod_near_nod_f_filter2(i) =   nnod_near_nod_f_filter(i)
        istack_near_nod_w_filter2(i) = istack_near_nod_w_filter(i)
        istack_near_nod_f_filter2(i) = istack_near_nod_f_filter(i)
      end do
!
      do i = 1, ntot_nod_near_w_filter
        inod_near_nod_w_filter2(i) = inod_near_nod_w_filter(i)
        whole_filter_func2(i) =      whole_filter_func(i)
        whole_filter_weight2(i) =    whole_filter_weight(i)
      end do
!
      do i = 1, ntot_nod_near_f_filter
        inod_near_nod_f_filter2(i) = inod_near_nod_f_filter(i)
        fluid_filter_func2(i) =      fluid_filter_func(i)
        fluid_filter_weight2(i) =    fluid_filter_weight(i)
      end do
!
      call deallocate_whole_filter_coefs
      call deallocate_fluid_filter_coefs
!
      end subroutine copy_new_filters_from_original
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine reorder_filter_new_domain
!
      integer(kind = kint) :: i, inod, inum
      integer(kind = kint) :: jnum_org, jnum_new
      integer(kind = kint) :: ist_org, ist_new
!
!
      inter_nod_3dfilter = intnod_w_fliter2
      ntot_nod_near_w_filter = ntot_nod_near_w_filter2
      ntot_nod_near_f_filter = ntot_nod_near_f_filter2
      call allocate_whole_filter_stack(inter_nod_3dfilter)
      call allocate_fluid_filter_stack(inter_nod_3dfilter)
      call allocate_whole_filter_coefs
      call allocate_fluid_filter_coefs
!
      istack_near_nod_w_filter(0) = istack_near_nod_w_filter2(0)
      istack_near_nod_f_filter(0) = istack_near_nod_f_filter2(0)
      do i = 1, inter_nod_3dfilter
        inod = inod_filter_new_2(i)
        i_exp_level_w_filter(inod) =     i_exp_level_w_filter2(i)
        i_exp_level_f_filter(inod) =     i_exp_level_f_filter2(i)
        nnod_near_nod_w_filter(inod) =   nnod_near_nod_w_filter2(i)
        nnod_near_nod_f_filter(inod) =   nnod_near_nod_f_filter2(i)
      end do
!
      do inod = 1, inter_nod_3dfilter
        istack_near_nod_w_filter(inod)                                  &
     &        = istack_near_nod_w_filter(inod-1)                        &
     &         + nnod_near_nod_w_filter(inod)
        if( nnod_near_nod_f_filter(inod) .le. 0) then
          istack_near_nod_f_filter(inod)                                &
     &        = istack_near_nod_f_filter(inod-1)
        else
          istack_near_nod_f_filter(inod)                                &
     &        = istack_near_nod_f_filter(inod-1)                        &
     &         + nnod_near_nod_f_filter(inod)
        end if
      end do
!
      do i = 1, inter_nod_3dfilter
        inod = inod_filter_new_2(i)
        ist_org = istack_near_nod_w_filter2(i-1)
        ist_new = istack_near_nod_w_filter(inod-1)
        do inum = 1, nnod_near_nod_w_filter(inod)
          jnum_org = inum + ist_org
          jnum_new = inum + ist_new
          inod_near_nod_w_filter(jnum_new)                              &
     &             = inod_near_nod_w_filter2(jnum_org)
          whole_filter_func(jnum_new)                                   &
     &             = whole_filter_func2(jnum_org)
          whole_filter_weight(jnum_new)                                 &
     &             = whole_filter_weight2(jnum_org)
        end do
!
        ist_org = istack_near_nod_f_filter2(i-1)
        ist_new = istack_near_nod_f_filter(inod-1)
        do inum = 1, nnod_near_nod_f_filter(inod)
          jnum_org = inum + ist_org
          jnum_new = inum + ist_new
          inod_near_nod_f_filter(jnum_new)                              &
     &             = inod_near_nod_f_filter2(jnum_org)
          fluid_filter_func(jnum_new)                                   &
     &             = fluid_filter_func2(jnum_org)
          fluid_filter_weight(jnum_new)                                 &
     &             = fluid_filter_weight2(jnum_org)
        end do
      end do
!
      call deallocate_whole_filter_coefs2
      call deallocate_fluid_filter_coefs2
!
      end subroutine reorder_filter_new_domain
!
!   --------------------------------------------------------------------
!
      end module copy_filters_4_sorting
