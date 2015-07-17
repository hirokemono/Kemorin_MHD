!m_merdional_grouping_patch.f90
!      module m_merdional_grouping_patch
!
!      written by Kemorin
!
!      subroutine read_med_grouping_patch(file_head, ele_grp)
!      subroutine deallocate_med_grouping_patch
!
      module m_merdional_grouping_patch
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), allocatable :: ie_egrp(:,:)
      real(kind = kreal), allocatable :: xx_egrp(:,:)
!
      private :: allocate_med_grouping_patch
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_med_grouping_patch(nele)
!
      integer(kind = kint),  intent(in) :: nele
!
      allocate( xx_egrp(3*nele,3) )
      allocate( ie_egrp(nele,3) )
      xx_egrp = 0.0d0
      ie_egrp = 0
!
      end subroutine allocate_med_grouping_patch
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_med_grouping_patch
!
!
      deallocate( xx_egrp, ie_egrp)
!
      end subroutine deallocate_med_grouping_patch
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_med_grouping_patch(file_head, ele_grp)
!
      use t_group_data
      use skip_comment_f
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint), parameter :: id_file = 20
      character(len=kchara) :: file_name
      character(len=255) :: character_4_read
!
      character(len=kchara) :: name_tmp
      integer(kind = kint) :: igrp, inod, iele, nnod, nele, inum
      integer(kind = kint) :: itmp, iloc(3)
!
!
      call add_dat_extension(file_head, file_name)
      write(*,*) 'ascii mesh file: ', trim(file_name)
      open (id_file, file = file_name, form = 'formatted')
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) ele_grp%num_grp
!      write(*,*) 'num_mat', ele_grp%num_grp
!
      ele_grp%num_item = 0
      do igrp = 1, ele_grp%num_grp
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) name_tmp
        read(id_file,*) nnod, nele
!        write(*,*) 'nnod, nele', igrp, nnod, nele
        ele_grp%num_item = ele_grp%num_item + nele
!
        do inod = 1, nnod
          read(id_file,*)  itmp
        end do
!
        if(nele .gt. 0) call skip_comment(character_4_read,id_file)
        do iele = 2, nele
          read(id_file,*)  itmp
        end do
      end do
!
      close(id_file)
!
      call allocate_grp_type(ele_grp)
      call allocate_med_grouping_patch(ele_grp%num_item)
!
!
      write(*,*) 'ascii mesh file: ', trim(file_name)
      open (id_file, file = file_name, form = 'formatted')
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) itmp
!
      do igrp = 1, ele_grp%num_grp
        call skip_comment(character_4_read,id_file)
        read(character_4_read,*) ele_grp%grp_name(igrp)
        write(*,*) 'mat_name: ', trim(ele_grp%grp_name(igrp))
        read(id_file,*) nnod, nele
        ele_grp%istack_grp(igrp) = ele_grp%istack_grp(igrp-1) + nele
!
        do inum = 1, nnod
          inod = 3*ele_grp%istack_grp(igrp-1) + inum
          read(id_file,*)  itmp, xx_egrp(inod,1:3)
        end do
!
        do inum = 1, nele
          iele = ele_grp%istack_grp(igrp-1) + inum
          call skip_comment(character_4_read,id_file)
          read(character_4_read,*)  itmp, iloc(1:3)
          ie_egrp(iele,1:3) = 3*ele_grp%istack_grp(igrp-1) + iloc(1:3)
        end do
      end do
!
      close(id_file)
!
      end subroutine read_med_grouping_patch
!
!  ---------------------------------------------------------------------
!
      end module m_merdional_grouping_patch
