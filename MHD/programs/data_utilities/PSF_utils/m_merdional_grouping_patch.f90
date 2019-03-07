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
      subroutine read_med_grouping_patch(file_head, ele_grp,            &
     &          start_name, istart_grp)
!
      use t_group_data
      use skip_comment_f
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head, start_name
      integer(kind = kint), intent(inout) :: istart_grp
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint), parameter :: id_gname = 20
      character(len=kchara) :: file_name
!
      integer(kind = kint) :: igrp, itmp
!
!
      file_name = add_dat_extension(file_head)
      open(id_gname,file=file_name)
      read(id_gname,*) ele_grp%num_grp
      call allocate_grp_type_num(ele_grp)
!
      do igrp = 1, ele_grp%num_grp
        read(id_gname,*) itmp, ele_grp%grp_name(igrp)
        if(ele_grp%grp_name(igrp) .eq. start_name) istart_grp = igrp
        write(*,*) igrp, ele_grp%grp_name(igrp), start_name
      end do
      close(id_gname)
!
      istart_grp = 1
      do igrp = 1, ele_grp%num_grp
        write(*,*) igrp, ele_grp%grp_name(igrp), start_name
        if(ele_grp%grp_name(igrp) .eq. start_name) then
          istart_grp = igrp
          exit
        end if
      end do
!
      end subroutine read_med_grouping_patch
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_elemental_field_2_ucd(mesh_file_head, ist_mesh,    &
     &          num_grp, nfld, ntot_comp, ncomp, fld_name, grp_data,    &
     &          ucd_med)
!
      use m_geometry_constants
      use t_ucd_data
      use set_parallel_file_name
      use write_ucd_to_vtk_file
!
      character(len=kchara), intent(in) :: mesh_file_head
      integer(kind = kint), intent(in) :: ist_mesh, num_grp
      integer(kind = kint), intent(in) :: nfld, ntot_comp
      integer(kind = kint), intent(in) :: ncomp(nfld)
      character(len=kchara), intent(in) :: fld_name(nfld)
      real(kind = kreal), intent(in) :: grp_data(num_grp,ntot_comp)
!
      type(ucd_data), intent(inout) :: ucd_med
!
      integer(kind = kint) :: igrp, nd
      integer(kind = kint_gl) :: inum, iele
      integer(kind = kint_gl) :: i1, i2, i3
      integer(kind = kint_gl), allocatable :: istack_ele(:)
!
      character(len=kchara) :: file_name, f_tmp
      type(ucd_data), allocatable :: grp_ucd(:)
!
!
      allocate(istack_ele(0:num_grp))
      allocate(grp_ucd(num_grp))
      do igrp = 1, num_grp
        f_tmp = add_int_suffix((igrp+ist_mesh-1), mesh_file_head)
        write(*,*) 'grp_ucd: ', ist_mesh, f_tmp
        file_name = set_parallel_grd_file_name(f_tmp, iflag_vtd, -1)
        call read_udt_data_2_vtk_grid(-1, file_name, grp_ucd(igrp))
      end do
!
!
      istack_ele(0) = 0
      do igrp = 1, num_grp
        istack_ele(igrp) = istack_ele(igrp-1) + grp_ucd(igrp)%nele
      end do
!
      ucd_med%nnod_4_ele = grp_ucd(1)%nnod_4_ele
      ucd_med%nele = istack_ele(num_grp)
      ucd_med%nnod = num_triangle * ucd_med%nele
      call allocate_ucd_node(ucd_med)
      call allocate_ucd_ele(ucd_med)
!
      do igrp = 1, num_grp
        do inum = 1, grp_ucd(igrp)%nele
          iele = inum + istack_ele(igrp-1)
          ucd_med%iele_global(iele) = iele
          ucd_med%ie(iele,1) = 3*iele-2
          ucd_med%ie(iele,2) = 3*iele-1
          ucd_med%ie(iele,3) = 3*iele
!
          i1 = grp_ucd(igrp)%ie(inum,1)
          ucd_med%inod_global(3*iele-2) = 3*iele-2
          ucd_med%xx(3*iele-2,1) = grp_ucd(igrp)%xx(i1,1)
          ucd_med%xx(3*iele-2,2) = grp_ucd(igrp)%xx(i1,2)
          ucd_med%xx(3*iele-2,3) = grp_ucd(igrp)%xx(i1,3)
!
          i2 = grp_ucd(igrp)%ie(inum,2)
          ucd_med%inod_global(3*iele-1) = 3*iele-1
          ucd_med%xx(3*iele-1,1) = grp_ucd(igrp)%xx(i2,1)
          ucd_med%xx(3*iele-1,2) = grp_ucd(igrp)%xx(i2,2)
          ucd_med%xx(3*iele-1,3) = grp_ucd(igrp)%xx(i2,3)
!
          i3 = grp_ucd(igrp)%ie(inum,3)
          ucd_med%inod_global(3*iele  ) = 3*iele
          ucd_med%xx(3*iele,  1) = grp_ucd(igrp)%xx(i3,1)
          ucd_med%xx(3*iele,  2) = grp_ucd(igrp)%xx(i3,2)
          ucd_med%xx(3*iele,  3) = grp_ucd(igrp)%xx(i3,3)
        end do
      end do
!
      ucd_med%num_field = nfld
      ucd_med%ntot_comp = ntot_comp
      call allocate_ucd_phys_name(ucd_med)
      call allocate_ucd_phys_data(ucd_med)
!
      ucd_med%num_comp(1:nfld) =  ncomp(1:nfld)
      ucd_med%phys_name(1:nfld) = fld_name(1:nfld)
!
      do igrp = 1, num_grp
        do inum = 1, grp_ucd(igrp)%nele
          iele = inum + istack_ele(igrp-1)
          do nd = 1, ntot_comp
            ucd_med%d_ucd(3*iele-2,nd) = grp_data(igrp,nd)
            ucd_med%d_ucd(3*iele-1,nd) = grp_data(igrp,nd)
            ucd_med%d_ucd(3*iele  ,nd) = grp_data(igrp,nd)
          end do
        end do
      end do
!
!      do igrp = 1, num_grp
!        call deallocate_ucd_ele(grp_ucd(igrp))
!        call deallocate_ucd_node(grp_ucd(igrp))
!      end do
!      deallocate(grp_ucd)
!
      deallocate(istack_ele)
!
      end subroutine set_elemental_field_2_ucd
!
!  ---------------------------------------------------------------------
!
      end module m_merdional_grouping_patch
