!filter_IO_for_newdomain
!      module filter_IO_for_newdomain
!
!     Written by H. Matsui on May, 2008
!
!!      subroutine write_new_whole_filter_coef                          &
!!     &         (file_name, whole_fil_sort, fil_coef)
!!      subroutine write_new_fluid_filter_coef                          &
!!     &         (file_name, fluid_fil_sort, fil_coef)
!!        type(filter_func_4_sorting), intent(in) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!
!!      subroutine read_filter_coef_4_newdomain                         &
!!     &         (id_file, fil_coef, whole_fil_sort, fluid_fil_sort)
!!      subroutine read_filter_coef_4_newdomain_b                       &
!!     &         (bin_flags, fil_coef, whole_fil_sort, fluid_fil_sort)
!!        type(binary_IO_flags), intent(inout) :: bin_flags
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!        type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
!!        type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      module filter_IO_for_newdomain
!
      use m_precision
!
      use t_filter_coefs
      use t_filter_func_4_sorting
      use m_nod_filter_comm_table
      use filter_IO_for_sorting
      use write_filters_4_each_node
!
      implicit none
!
      type(filter_func_4_sorting) :: tmp_fil_sort
!
      private :: set_filter_item_4_IO
      private :: set_w_filter_item_4_newdomain
      private :: set_f_filter_item_4_newdomain
      private :: set_filter_item_4_newdomain
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_new_whole_filter_coef                            &
     &         (file_name, whole_fil_sort, fil_coef)
!
      character(len = kchara), intent(in) :: file_name
      type(filter_func_4_sorting), intent(in) :: whole_fil_sort
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: inod, ierr
!
!
      do inod = 1, inter_nod_3dfilter
        call set_filter_item_4_IO(inod, whole_fil_sort, fil_coef)
        call write_each_filter_stack_coef                               &
     &     (file_name, inod, fil_coef, ierr)
      end do
!
      end subroutine write_new_whole_filter_coef
!
!  ---------------------------------------------------------------------
!
      subroutine write_new_fluid_filter_coef                            &
     &         (file_name, fluid_fil_sort, fil_coef)
!
      character(len = kchara), intent(in) :: file_name
      type(filter_func_4_sorting), intent(in) :: fluid_fil_sort
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: inod, ierr
!
!
      do inod = 1, inter_nod_3dfilter
        call set_filter_item_4_IO(inod, fluid_fil_sort, fil_coef)
        if (fil_coef%nnod_4_1nod_w .lt. 0) then
          fil_coef%nnod_4_1nod_w = -fil_coef%nnod_4_1nod_w
          call write_each_same_filter_coef                              &
     &       (file_name, inod, fil_coef, ierr)
        else if(fil_coef%nnod_4_1nod_w .eq. 0) then
          call write_each_no_filter_coef                                &
     &       (file_name, inod, fil_coef, ierr)
        else
          call write_each_filter_stack_coef                             &
     &       (file_name, inod, fil_coef, ierr)
        end if
      end do
!
      end subroutine write_new_fluid_filter_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_newdomain                           &
     &         (id_file, fil_coef, whole_fil_sort, fluid_fil_sort)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      integer(kind = kint) :: inod
!
!
      whole_fil_sort%ntot_nod_near_filter = 0
      call alloc_filter_func_4_sort(whole_fil_sort)
      call alloc_filter_num_4_sort(inter_nod_3dfilter, whole_fil_sort)
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each(id_file, fil_coef)
        call set_w_filter_item_4_newdomain                              &
     &     (inod, fil_coef, whole_fil_sort)
      end do
!
      fluid_fil_sort%ntot_nod_near_filter = 0
      call alloc_filter_num_4_sort(inter_nod_3dfilter, fluid_fil_sort)
      call alloc_filter_func_4_sort(fluid_fil_sort)
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each(id_file, fil_coef)
        call set_f_filter_item_4_newdomain                              &
     &     (inod, fil_coef, fluid_fil_sort)
      end do
!
      end subroutine read_filter_coef_4_newdomain
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_coef_4_newdomain_b                         &
     &         (bin_flags, fil_coef, whole_fil_sort, fluid_fil_sort)
!
      type(binary_IO_flags), intent(inout) :: bin_flags
      type(each_filter_coef), intent(inout) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: whole_fil_sort
      type(filter_func_4_sorting), intent(inout) :: fluid_fil_sort
!
      integer(kind = kint) :: inod
!
!
      whole_fil_sort%ntot_nod_near_filter = 0
      call alloc_filter_func_4_sort(whole_fil_sort)
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each_b(bin_flags, fil_coef)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call set_w_filter_item_4_newdomain                              &
     &     (inod, fil_coef, whole_fil_sort)
      end do
!
!
      fluid_fil_sort%ntot_nod_near_filter = 0
      call alloc_filter_func_4_sort(fluid_fil_sort)
!
      do inod = 1, inter_nod_3dfilter
        call read_filter_coef_4_each_b(bin_flags, fil_coef)
        if(bin_flags%ierr_IO .gt. 0) return
!
        call set_f_filter_item_4_newdomain                              &
     &     (inod, fil_coef, fluid_fil_sort)
      end do
!
      end subroutine read_filter_coef_4_newdomain_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_filter_item_4_IO(inod, f_sorting, fil_coef)
!
      integer(kind = kint), intent(in) :: inod
      type(filter_func_4_sorting), intent(in) :: f_sorting
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: i, j
!
!
      fil_coef%ilevel_exp_1nod_w                                        &
     &     = f_sorting%i_exp_level_filter(inod)
      fil_coef%nnod_4_1nod_w                                            &
     &     = f_sorting%nnod_near_nod_filter(inod)
      do i = 1, fil_coef%nnod_4_1nod_w
        j = f_sorting%istack_near_nod_filter(inod-1) + i
        fil_coef%inod_4_1nod_w(i)                                       &
     &     = f_sorting%inod_near_nod_filter(j)
        fil_coef%filter_1nod(i) =   f_sorting%filter_func(j)
        fil_coef%weight_1nod(i) =   f_sorting%filter_weight(j)
      end do
!
      end subroutine set_filter_item_4_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_w_filter_item_4_newdomain                          &
               (inod, fil_coef, f_sorting)
!
      integer(kind = kint), intent(in) :: inod
      type(each_filter_coef), intent(in) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
!
      f_sorting%i_exp_level_filter(inod) =   fil_coef%ilevel_exp_1nod_w
      f_sorting%nnod_near_nod_filter(inod) = fil_coef%nnod_4_1nod_w
      f_sorting%istack_near_nod_filter(inod)                            &
     &    = f_sorting%istack_near_nod_filter(inod-1)                    &
     &     + fil_coef%nnod_4_1nod_w
!
      call set_filter_item_4_newdomain(inod, fil_coef, f_sorting)
!
      end subroutine set_w_filter_item_4_newdomain
!
!  ---------------------------------------------------------------------
!
      subroutine set_f_filter_item_4_newdomain                          &
     &         (inod, fil_coef, f_sorting)
!
      integer(kind = kint), intent(in) :: inod
      type(each_filter_coef), intent(in) :: fil_coef
!
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
!
      f_sorting%i_exp_level_filter(inod)                                &
     &    = fil_coef%ilevel_exp_1nod_w
      f_sorting%nnod_near_nod_filter(inod)                              &
     &    = fil_coef%nnod_4_1nod_w
      if (fil_coef%nnod_4_1nod_w .le. 0) then
        f_sorting%istack_near_nod_filter(inod)                          &
     &      = f_sorting%istack_near_nod_filter(inod-1)
      else
        f_sorting%istack_near_nod_filter(inod)                          &
     &      = f_sorting%istack_near_nod_filter(inod-1)                  &
     &       + fil_coef%nnod_4_1nod_w
!
        call set_filter_item_4_newdomain(inod, fil_coef, f_sorting)
      end if
!
      end subroutine set_f_filter_item_4_newdomain
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_filter_item_4_newdomain(inod, fil_coef, f_sorting)
!
      integer(kind = kint), intent(in) :: inod
      type(each_filter_coef), intent(in) :: fil_coef
      type(filter_func_4_sorting), intent(inout) :: f_sorting
!
      integer(kind = kint) :: i, j
!
!
      tmp_fil_sort%ntot_nod_near_filter                                 &
     &      = f_sorting%ntot_nod_near_filter
      call alloc_filter_func_4_sort(tmp_fil_sort)
!
      call copy_filter_func_4_sort                                      &
     &  (ione, f_sorting%ntot_nod_near_filter, f_sorting, tmp_fil_sort)
!
      call dealloc_filter_num_4_sort(f_sorting)
!
      f_sorting%ntot_nod_near_filter                                    &
     &    = f_sorting%istack_near_nod_filter(inod)
      call alloc_filter_func_4_sort(f_sorting)
!
      call copy_filter_func_4_sort                                      &
     &  (ione, f_sorting%ntot_nod_near_filter, tmp_fil_sort, f_sorting)
!
      do i = 1, fil_coef%nnod_4_1nod_w
        j = f_sorting%istack_near_nod_filter(inod-1) + i
        f_sorting%inod_near_nod_filter(j)                               &
     &     = fil_coef%inod_4_1nod_w(i)
        f_sorting%filter_func(j) =   fil_coef%filter_1nod(i)
        f_sorting%filter_weight(j) = fil_coef%weight_1nod(i)
      end do
!
      call dealloc_filter_func_4_sort(tmp_fil_sort)
!
      end subroutine set_filter_item_4_newdomain
!
!  ---------------------------------------------------------------------
!
      end module filter_IO_for_newdomain
