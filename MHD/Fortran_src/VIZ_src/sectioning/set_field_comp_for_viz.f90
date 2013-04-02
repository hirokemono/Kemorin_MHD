!set_field_comp_for_viz.f90
!      module set_field_comp_for_viz
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine check_field_4_viz(num_nod_phys, phys_nod_name,        &
!     &          n_field_ctl, field_name, num_field)
!      subroutine set_components_4_viz(num_nod_phys, phys_nod_name,     &
!     &          n_field_ctl, field_name, comp_name, num_field,         &
!     &          ifield, icomp, ncomp, ncomp_org, rst_name)
!
!      subroutine count_total_comps_4_viz(num, num_total, istack_out,   &
!     &      ncomp_out, ncomp_total_out, num_out_comp, istack_out_comp)
!
      module set_field_comp_for_viz
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_field_4_viz(num_nod_phys, phys_nod_name,         &
     &          n_field_ctl, field_name, num_field)
!
      integer(kind = kint), intent(in) :: num_nod_phys, n_field_ctl
!
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      character(len=kchara), intent(in) :: field_name(n_field_ctl)
!
      integer(kind = kint), intent(inout) :: num_field
!
      integer(kind = kint) :: i, id
!
!
      num_field = 0
      do i = 1, n_field_ctl
        do id = 1, num_nod_phys
          if ( field_name(i) .eq. phys_nod_name(id) ) then
            num_field = num_field + 1
            exit
          end if
        end do
      end do
!
      end subroutine check_field_4_viz
!
!  ---------------------------------------------------------------------
!
      subroutine set_components_4_viz(num_nod_phys, phys_nod_name,      &
     &          n_field_ctl, field_name, comp_name, num_field,          &
     &          ifield, icomp, ncomp, ncomp_org, rst_name)
!
      use set_components_flags
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: n_field_ctl
      integer(kind = kint), intent(in) :: num_field
      character(len=kchara), intent(in) :: field_name(n_field_ctl)
      character(len=kchara), intent(in) :: comp_name(n_field_ctl)
!
      integer(kind = kint), intent(inout) :: ifield(num_field)
      integer(kind = kint), intent(inout) :: icomp(num_field)
      integer(kind = kint), intent(inout) :: ncomp(num_field)
      integer(kind = kint), intent(inout) :: ncomp_org(num_field)
      character(len=kchara), intent(inout) :: rst_name(num_field)
!
      integer(kind = kint) :: i, id, icou
!
!
      icou = 0
      do i = 1, n_field_ctl
        do id = 1, num_nod_phys
          if ( field_name(i) .eq. phys_nod_name(id) ) then
!
            icou = icou + 1
            ifield(icou) = id
!
            call s_set_components_flags( comp_name(i), field_name(i),   &
     &          icomp(icou), ncomp(icou), ncomp_org(icou),              &
     &          rst_name(icou))
            exit
          end if
        end do
      end do
!
      end subroutine set_components_4_viz
!
!  ---------------------------------------------------------------------
!
      subroutine count_total_comps_4_viz(num, num_total, istack_out,    &
     &      ncomp_out, max_ncomp_out, num_out_comp)
!
      integer(kind = kint), intent(in) :: num, num_total
      integer(kind = kint), intent(in) :: istack_out(0:num)
      integer(kind = kint), intent(in) :: ncomp_out(num_total)
!
      integer(kind = kint), intent(inout) :: max_ncomp_out
      integer(kind = kint), intent(inout) :: num_out_comp(num)
!
      integer(kind = kint) :: i, j
!
!
      num_out_comp = 0
      max_ncomp_out = 3
      do i = 1, num
        do j = istack_out(i-1)+1, istack_out(i)
          num_out_comp(i) = num_out_comp(i) + ncomp_out(j)
        end do
        max_ncomp_out = max(max_ncomp_out,num_out_comp(i))
      end do
!
      end subroutine count_total_comps_4_viz
!
!  ---------------------------------------------------------------------
!
      end module set_field_comp_for_viz
