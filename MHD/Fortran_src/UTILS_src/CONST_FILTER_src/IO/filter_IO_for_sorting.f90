!
!      module filter_IO_for_sorting
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine read_filter_neib_4_sort                              &
!!     &         (id_file, fil_area, f_sorting, nmax_nod_near_all)
!!         type(filter_area_flag), intent(inout) :: fil_area
!!         type(filter_func_4_sorting), intent(inout) :: f_sorting
!!
!!      subroutine read_filter_coef_4_sort                              &
!!     &         (id_file, whole_area, fluid_area, fil_coef, fil_sorted)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!        type(filter_coefficients_type), intent(inout) :: fil_sorted
!!
!!      subroutine write_filter_coef_4_each(id_file, fil_coef)
!!        type(each_filter_coef), intent(in) :: fil_coef
!!      subroutine read_filter_coef_4_each(id_file, fil_coef)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!
!!      subroutine read_filter_neib_4_sort_b                            &
!!     &         (bbuf, fil_area, f_sorting, nmax_nod_near_all)
!!         type(binary_IO_buffer), intent(inout) :: bbuf
!!         type(filter_area_flag), intent(inout) :: fil_area
!!         type(filter_func_4_sorting), intent(inout) :: f_sorting
!!      subroutine read_filter_coef_4_sort_b                            &
!!     &         (bbuf, filter, whole_area, fluid_area,                 &
!!     &          fil_coef, fil_sorted)
!!      subroutine write_filter_coef_4_each_b(fil_coef, bbuf)
!!        type(each_filter_coef), intent(in) :: fil_coef
!!        type(filter_coefficients_type), intent(inout) :: fil_sorted
!!      subroutine read_filter_coef_4_each_b(bbuf, fil_coef)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(each_filter_coef), intent(inout) :: fil_coef
!
      module filter_IO_for_sorting
!
      use m_precision
!
      use t_filter_coefs
      use t_filter_coefficients
      use t_filter_func_4_sorting
      use t_binary_IO_buffer
      use m_nod_filter_comm_table
      use binary_IO
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
      private :: set_filtering_item_4_sorting
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_neib_4_sort                                &
     &         (id_file, fil_area, f_sorting, nmax_nod_near_all)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(inout) :: nmax_nod_near_all
      type(filter_area_flag), intent(inout) :: fil_area
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
      integer(kind = kint) :: inod, itmp, jnod
!
!
      call alloc_filter_num_sort(inter_nod_3dfilter, fil_area)
      call alloc_filter_num_4_sort(inter_nod_3dfilter, f_sorting)
!
      do inod = 1, inter_nod_3dfilter
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*)                                        &
     &        itmp, f_sorting%nnod_near_nod_filter(inod),               &
     &        fil_area%i_exp_level(inod)
        nmax_nod_near_all                                               &
     &   = max(nmax_nod_near_all, f_sorting%nnod_near_nod_filter(inod))
        do jnod = 1, f_sorting%nnod_near_nod_filter(inod)
          read(id_file,*) itmp
        end do
      end do
!
      end subroutine read_filter_neib_4_sort
!
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_sort                                &
     &         (id_file, whole_area, fluid_area, fil_coef, fil_sorted)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_area_flag), intent(in) :: whole_area, fluid_area
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_coefficients_type), intent(inout) :: fil_sorted
!
      integer(kind = kint) :: inod, icou
!
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each(id_file, fil_coef)
!
        icou = whole_area%itbl_near_nod(inod)
        call set_filtering_item_4_sorting(icou, fil_coef, fil_sorted)
      end do
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each(id_file, fil_coef)
!
        icou = fluid_area%itbl_near_nod(inod)
        call set_filtering_item_4_sorting(icou, fil_coef, fil_sorted)
      end do
!
      end subroutine read_filter_coef_4_sort
!
!  ---------------------------------------------------------------------
!
      subroutine set_filtering_item_4_sorting                           &
     &         (icou, fil_coef, fil_sorted)
!
      integer(kind = kint), intent(in) :: icou
      type(each_filter_coef), intent(in) :: fil_coef
      type(filter_coefficients_type), intent(inout) :: fil_sorted
!
      integer(kind = kint) :: i, j, ist_nod
!
!
        ist_nod = fil_sorted%istack_near_nod(icou-1)
        do i = 1, fil_coef%nnod_4_1nod_w
          j = ist_nod + i
          fil_sorted%inod_near(j) = fil_coef%inod_4_1nod_w(i)
          fil_sorted%func(j) =   fil_coef%filter_1nod(i)
          fil_sorted%weight(j) = fil_coef%weight_1nod(i)
        end do
!
      end subroutine set_filtering_item_4_sorting
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_filter_coef_4_each(id_file, fil_coef)
!
      integer(kind = kint), intent(in) :: id_file
      type(each_filter_coef), intent(in) :: fil_coef
!
      integer(kind = kint) :: inum
!
!
      if (fil_coef%nnod_4_1nod_w .gt. 0) then
        do inum = 1, fil_coef%nnod_4_1nod_w
          write(id_file,'(2i12,1p2E25.15e3)') inum,                     &
     &      fil_coef%inod_4_1nod_w(inum), fil_coef%filter_1nod(inum),   &
     &      fil_coef%weight_1nod(inum)
        end do
      end if
!
      end subroutine write_filter_coef_4_each
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_each(id_file, fil_coef)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: inum, itmp
      character(len=255) :: character_4_read = ''
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*)  itmp, fil_coef%nnod_4_1nod_w,           &
     &                                fil_coef%ilevel_exp_1nod_w
!
      if (fil_coef%nnod_4_1nod_w.gt. 0) then
        do inum = 1, fil_coef%nnod_4_1nod_w
          read(id_file,*) itmp,                                         &
     &      fil_coef%inod_4_1nod_w(inum), fil_coef%filter_1nod(inum),   &
     &      fil_coef%weight_1nod(inum)
        end do
      end if
!
      end subroutine read_filter_coef_4_each
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_neib_4_sort_b                              &
     &         (bbuf, fil_area, f_sorting, nmax_nod_near_all)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(filter_area_flag), intent(inout) :: fil_area
      type(filter_func_4_sorting), intent(inout) :: f_sorting
      integer(kind = kint), intent(inout) :: nmax_nod_near_all
!
      integer(kind = kint) :: inod
      integer(kind = kint_gl) :: ioffset
!
!
      call alloc_filter_num_sort(inter_nod_3dfilter, fil_area)
      call alloc_filter_num_4_sort(inter_nod_3dfilter, f_sorting)
!
      do inod = 1, inter_nod_3dfilter
        call read_one_integer_b                                         &
     &     (bbuf, f_sorting%nnod_near_nod_filter(inod))
        if(bbuf%ierr_bin .gt. 0) return
!
        call read_one_integer_b(bbuf, fil_area%i_exp_level(inod))
        if(bbuf%ierr_bin .gt. 0) return
!
        ioffset = f_sorting%nnod_near_nod_filter(inod) * (kint+2*kreal)
        call seek_forward_binary_file(ioffset, bbuf)
        nmax_nod_near_all                                               &
     &    = max(nmax_nod_near_all,f_sorting%nnod_near_nod_filter(inod))
      end do
!
      end subroutine read_filter_neib_4_sort_b
!
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_sort_b                              &
     &         (bbuf, filter, whole_area, fluid_area,                   &
     &          fil_coef, fil_sorted)
!
      use t_filter_coefficients
!
      type(filter_coefficients_type), intent(in) :: filter
      type(filter_area_flag), intent(in) :: whole_area, fluid_area
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_coefficients_type), intent(inout) :: fil_sorted
!
      integer(kind = kint) :: inod, icou
!
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each_b(bbuf, fil_coef)
        if(bbuf%ierr_bin .gt. 0) return
!
        icou = whole_area%itbl_near_nod(inod)
        call set_filtering_item_4_sorting(icou, fil_coef, fil_sorted)
      end do
!
      do inod = 1, inter_nod_3dfilter
        if (icou .le. filter%ntot_nod) then
          call read_filter_coef_4_each_b(bbuf, fil_coef)
          if(bbuf%ierr_bin .gt. 0) return
!
          icou = fluid_area%itbl_near_nod(inod)
          call set_filtering_item_4_sorting(icou, fil_coef, fil_sorted)
        end if
      end do
!
      end subroutine read_filter_coef_4_sort_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_coef_4_each_b(fil_coef, bbuf)
!
      type(each_filter_coef), intent(in) :: fil_coef
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(fil_coef%nnod_4_1nod_w, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(fil_coef%ilevel_exp_1nod_w, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      num64 = fil_coef%nnod_4_1nod_w
      call write_mul_integer_b(num64, fil_coef%inod_4_1nod_w, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_1d_vector_b(num64, fil_coef%filter_1nod, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_1d_vector_b(num64, fil_coef%weight_1nod, bbuf)
!
      end subroutine write_filter_coef_4_each_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_each_b(bbuf, fil_coef)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint_gl) :: num64
!
!
      call read_one_integer_b(bbuf, fil_coef%nnod_4_1nod_w)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_one_integer_b(bbuf, fil_coef%ilevel_exp_1nod_w)
      if(bbuf%ierr_bin .gt. 0) return
!
      num64 = fil_coef%nnod_4_1nod_w
      call read_mul_integer_b(bbuf, num64, fil_coef%inod_4_1nod_w)
      if(bbuf%ierr_bin .gt. 0) return
      call read_1d_vector_b(bbuf, num64, fil_coef%filter_1nod)
      if(bbuf%ierr_bin .gt. 0) return
      call read_1d_vector_b(bbuf, num64, fil_coef%weight_1nod)
!
      end subroutine read_filter_coef_4_each_b
!
!  ---------------------------------------------------------------------
!
      end module filter_IO_for_sorting
